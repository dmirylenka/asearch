(ns scienscan.handler
  (:use compojure.core
        scienscan.core)
  (:require
   [clojure.string :as string]
   [compojure.handler :as handler]
   [compojure.route :as route]
   [noir.session :as session]
   [cheshire [core :as json]]
   [utils.core :as u]
   [search-api.search-api :as sapi]
   [mas-api.core :as mas]
   [arxiv-api.core :as arxiv]
   [arnetminer.dataset :as aminer]
   [topic-maps.core :as tmaps]
   [scienscan [html :as html]
              [submaps :as submaps]
              [conf :as conf]]))

(def config scienscan.conf/conf)

(def options (config :scienscan-opt))

(def search-service (apply aminer/service (apply concat (config :aminer-opt))))

(def n-labels (options :n-labels))

(def n-candidates (options :n-candidates))

(def n-dagger-iter (options :n-dagger-iter))

(defn- cache-filename [query]
  (str "./resources/data/query-cache/" query ".clj"))

(defn cache-data!
 ([data]
  (cache-data! data (:query data)))
 ([data query]
  (do
   (spit (cache-filename query) (dissoc data :summarizer))
   data)))

(defn- cached-data [query]
  (read-string (slurp (cache-filename query))))

(def query-cache
  (u/val-map cached-data [(:query options)]))

(defn validate [{:keys [query n-topics topic-id ban-topic-id] :as params}]
  (let [default-params (-> options
                           (select-keys [:query :n-topics])
                           (update-in [:n-topics] str))]
    (-> params
        (cond-> (string/blank? query) (dissoc :query)
                (nil? n-topics) (dissoc :n-topics))
        (cond-> (not (nil? topic-id))
                (update-in [:topic-id] #(Integer/parseInt %)))
        (cond-> (not (nil? ban-topic-id))
                (update-in [:ban-topic-id] #(Integer/parseInt %)))
        (#(merge default-params %))
        (update-in [:n-topics] #(Integer/parseInt %)))))

(defn mk-paper [{:keys [id author title abstract year] :as paper}]
  (let [doc-id (str title " " year " " (doall (string/join ", " (map :full-name author))))
        doc-string (str title ". " abstract)]
    (-> paper
        (assoc :doc-id doc-id
               :string doc-string))))

(defn get-query-results [query]
  {:pre [(not (string/blank? query))]}
  (let [timeout (options :search-timeout)
        n-results (options :n-results)]
    (u/fmap
     (sapi/search-papers search-service query :end n-results :timeout timeout)
     (partial map mk-paper))))

(defn build-topic-map [results]
  {:pre [(instance? utils.core.Result results)]}
  (u/fmap results submaps/build-topic-map))

(defn get-summary [{:keys [summarizer topic-map n-topics] :as data}]
  {:pre [(instance? utils.core.Result topic-map)
         (instance? utils.core.Result summarizer)
         (not (nil? n-topics))
         (or (u/fail? topic-map)
             (>= (count (tmaps/get-topics (:value topic-map))) n-topics))]}
  (u/fmap summarizer submaps/get-summary))

(defn compute-svg [submap]
  {:pre [(instance? utils.core.Result submap)]}
  (u/fmap submap
    (fn [sbm]
      (let [candidate? (or (:candidate-topics sbm)
                           (constantly false))
            node-color (fn [topic]
                       (if (candidate? topic)
                         "#aaaaaa"
                         "#eeeeff"))
            node-style (fn [topic]
                         (if (candidate? topic)
                           "rounded,dashed"
                           "rounded,filled"))
            line-style (fn [topic1 topic2]
                         (if (or (candidate? topic1)
                                 (candidate? topic2))
                           "dashed"
                           "solid"))
            line-color (fn [topic1 topic2]
                         (if (or (candidate? topic1)
                                 (candidate? topic2))
                           "#aaaaaa"
                           "#9999bb"))]
        (tmaps/topics2svg sbm
                          :node-color node-color
                          :node-style node-style
                          :line-color line-color
                          :line-style line-style)))))

(defn build-js-data [{:keys [topic-map interface complete-labeling]}]
 [topic-map]
 (u/fmap topic-map
   (fn [topic-map]
     {:pre [(not (nil? topic-map))
            (instance? topic_maps.core.TopicMap topic-map)]}
     (let [topics (tmaps/get-topics topic-map)
           topic-titles (->> topics
                             (u/val-map (partial tmaps/proper-docs topic-map))
                             (u/map-key :id))
           rel (for [topic topics
                     child-topic (tmaps/child-topics topic-map topic)]
                 (mapv (comp str :id) [child-topic topic]))
           title-results (:doc-map topic-map)]
       {:topic-titles topic-titles
        :rel rel
        :title-results title-results
        :topic-index (u/key-map :id topics)
        :interface interface
        :complete-labeling complete-labeling}))))

(def -summarizer
  (->> submaps/dagger-summarizer
      (submaps/retrain-summarizer n-dagger-iter)
      u/->Success))

(def -labeling-summarizer (u/->Success ;; (submaps/retrain-summarizer
                           ;n-dagger-iter
                           submaps/dagger-summarizer
                           ));)

;; (defn fresh-summarizer (u/->Success submaps/dagger-summarizer))

(defn get-ntopics-max [topic-map]
  (if (u/success? topic-map)
    (count (tmaps/get-topics (:value topic-map)))
    0))

(defn build-topic-index [topic-map]
  (u/fmap topic-map #(u/key-map :id (tmaps/get-topics %))))

(defn new-topic-map-data [query]
  {:pre [(not (string/blank? query))]}
  (println "Preparing new data:" query)
  (-> {:query query}
      (u/assocf get-query-results :query :results)
      (u/assocf build-topic-map :results :topic-map)
      (u/assocf get-ntopics-max :topic-map :n-topics-max)
      (u/assocf build-topic-index :topic-map :topic-index)
      (assoc :summarizer -summarizer)))

(defn get-session-data [query]
  (let [data (session/get :session-data)]
    (when (and (= query (:query data))
               (u/success? (:topic-map data)))
      (println "Hit the session data:" query)
      data)))

(defn get-query-cache [query]
  (when-let [data (query-cache query)]
    (println "Hit the query cache:" query)
    ;; (let [topic-map (:value (:topic-map data))
    ;;       topics (tmaps/get-topics topic-map)]
    ;;   (println "There are nil topics:" (some nil? topics))
    ;;   (println (string/join "; " (map :title topics)))
      (-> data
          (assoc :summarizer -summarizer)
          (u/assocf build-topic-index :topic-map :topic-index))));)

(defn topic-map-data [query]
  (or (get-session-data query)
      (get-query-cache query)
      (new-topic-map-data query)))

(defn build-summary
  "Returns the updated summarizer with the computed summary."
  [{:keys [summarizer topic-map n-topics] :as data}]
  {:pre [(instance? utils.core.Result topic-map)
         (not (nil? n-topics))
         (or (u/fail? topic-map)
             (>= (count (tmaps/get-topics (:value topic-map))) n-topics))]}
  (u/bind summarizer (fn [smr] (u/fmap topic-map #(submaps/build-summary smr % n-topics)))))

(defn init-with-topic-map [{:keys [topic-map labeling-summarizer]}]
  (u/bind labeling-summarizer
    (fn [smr]
      (u/fmap topic-map
        (fn [tm]
          (cond-> smr
                  (nil? (submaps/get-summary smr))
                  (submaps/build-summary tm 0)))))))

(defn add-candidates-to-summary
  "Adds the candidate next topics to the summary graph."
  [{:keys [labeling-summarizer topic-map banned-topics complete-labeling] :as data}]
  {:pre [(instance? utils.core.Result topic-map)]}
  (u/fmap labeling-summarizer
    (fn [smr]
      (u/fmap topic-map
        (fn [tm]
          (let [submap (submaps/get-summary smr)
                topics (tmaps/get-topics submap)
                _ (println "Current topcis in the submap:" (string/join "; " (map :title topics)))
                candidate-topics (when-not complete-labeling  ;; limiting the number of collected topics
                                   (submaps/get-k-candidates smr n-candidates (or banned-topics #{})))
                _ (println "Candidate topics:" (string/join "; " (map :title candidate-topics)))]
            (-> (tmaps/submap tm (into topics candidate-topics))
                (assoc :candidate-topics (set candidate-topics)))))))))

(defn prepare-data [{:keys [query n-topics interface] :as request}]
  {:pre [(not (string/blank? query))
         (not (nil? n-topics))]}
  (-> request
      (merge (topic-map-data query))
      (assoc :n-topics n-topics
             :interface interface)
      (u/assocf build-summary identity :summarizer)
      (u/assocf get-summary identity :submap)
      (u/assocf compute-svg :submap :svg)
      (u/assocf build-js-data identity :json)))

(defn add-next-topic [{:keys [topic-id topic-index labeling-summarizer] :as data}]
  (cond-> data
    (and topic-index topic-id)
    (assoc :labeling-summarizer
      (u/bind topic-index
        (fn [ti]
          (u/fmap labeling-summarizer
            (fn [smr]
              (submaps/add-next-topic smr (ti topic-id)))))))))

(defn ban-next-topic [{:keys [ban-topic-id banned-topics topic-index] :as data}]
  (cond-> data
    (and (u/success? topic-index) ban-topic-id)
    (assoc :banned-topics
      (conj (or banned-topics #{})
            ((:value topic-index) ban-topic-id)))))

(defn complete-labeling? [summarizer]
  (when-let [summarizer (:value summarizer)]
    (let [submap (submaps/get-summary summarizer)
          topics (tmaps/get-topics submap)]
      (>= (count topics) n-labels))))

(defn refresh-labeling-summarizer [summarizer]
  (if (nil? summarizer)
    -labeling-summarizer
    (u/fmap summarizer submaps/refresh-summarizer)))

(defn prepare-labeling-data [{:keys [query n-topics topic-id ban-topic-id topic-index interface] :as request}]
  {:pre [(not (string/blank? query))]}
  (-> request
      (merge (topic-map-data query))
      (assoc :topic-id topic-id
             :ban-topic-id ban-topic-id
             :interface interface)
      (cond-> (nil? (or topic-id ban-topic-id))
              (update-in [:labeling-summarizer] refresh-labeling-summarizer)) ;; clear the summarizer
      (u/assocf init-with-topic-map identity :labeling-summarizer)
      (cond-> topic-id add-next-topic
              ban-topic-id ban-next-topic)
      (cond-> topic-id
              (update-in [:labeling-summarizer] #(u/fmap % (partial submaps/retrain-summarizer n-dagger-iter))))
      (u/assocf complete-labeling? :labeling-summarizer :complete-labeling)
      (u/assocf add-candidates-to-summary identity :submap)
      (u/assocf compute-svg :submap :svg)
      (u/assocf build-js-data identity :json)))

(defn save-ground-truth! [{:keys [labeling-summarizer complete-labeling] :as data}]
  (when (and complete-labeling (:value labeling-summarizer))
    (submaps/save-ground-truth! (:value labeling-summarizer))))
  
(defn render-json-svg [{:keys [json svg]}]
  (json/encode
   {:svg (:value svg)
    :json (:value json)}))

(defn render-search-page [data]
  (html/layout (html/search-page-html data)))

(defn render-labeling-page [data]
  (apply html/layout (html/labeling-page-html data)))

(defroutes app-routes
  (GET "/" [query n-topics]
       (println "Request:" query n-topics)
       (println (session/get query))
       (-> {:query query :n-topics n-topics :interface :search}
           validate
           prepare-data
           (doto (#(session/put! :session-data %)))
           ;; cache-data!
           render-search-page))
  (POST "/refine" [query n-topics]
        (println query n-topics)
        (-> {:query query :n-topics n-topics :interface :search}
            validate
            prepare-data
           (doto (#(session/put! :session-data %)))
            render-json-svg))
  (GET "/labeling" [query]
    (println "Labeling request: " query)
    (-> {:query query :interface :labeling}
        validate
        prepare-labeling-data
        (doto (#(session/put! :session-data %)))
        render-labeling-page))
  (POST "/labeling" [query topic-id ban-topic-id]
    (println "Labeling request: " query topic-id)
    (-> {:query query :topic-id topic-id :interface :labeling}
        validate
        prepare-labeling-data
        (doto (#(save-ground-truth! %)))
        (doto (#(session/put! :session-data %)))
        render-json-svg))
  (POST "/ban" [query topic-id]
    (println "Ban request: " query topic-id)
    (-> {:query query :ban-topic-id topic-id :interface :labeling}
        validate
        prepare-labeling-data
        (doto (#(session/put! :session-data %)))
        render-json-svg))
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (-> (handler/site app-routes)
      (session/wrap-noir-session)))
