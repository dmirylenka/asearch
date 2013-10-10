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
   [scienscan.html :as html]
   [scienscan.submaps :as submaps]))

(def options
  {:n-topics 5
   :query "statistical relational learning"
   :ms-timeout 30000
   :n-results 100
   :submap-fn submaps/best-by-greedy-coverage})

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

(defn validate [{:keys [query n-topics] :as params}]
  (let [default-params (-> options
                           (select-keys [:query :n-topics])
                           (update-in [:n-topics] str))]
    (-> params
        (cond-> (string/blank? query) (dissoc :query)
                (nil? n-topics) (dissoc :n-topics))
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
  (let [timeout (options :ms-timeout)
        n-results (options :n-results)]
    (u/fmap
     (sapi/search-papers aminer/service query :end n-results :timeout timeout)
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
  (u/fmap submap tmaps/topics2svg))

(defn build-js-data [topic-map]
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
     :topic-index (u/key-map :id topics)}))

(def -summarizer (u/->Success submaps/dagger-summarizer))

(defn new-topic-map-data [query]
  {:pre [(not (string/blank? query))]}
  (println "Preparing new data:" query)
  (-> {:query query}
      (u/assocf get-query-results :query :results)
      (u/assocf build-topic-map :results :topic-map)
      (u/assocf count (comp tmaps/get-topics :value :topic-map) :n-topics-max)
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
    (assoc 
        data
      :summarizer -summarizer)
))

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

(defn prepare-data [{:keys [query n-topics] :as request}]
  {:pre [(not (string/blank? query))
         (not (nil? n-topics))]}
  (-> request
      (merge (topic-map-data query))
      (assoc :n-topics n-topics)
      (u/assocf build-summary identity :summarizer)
      (u/assocf get-summary identity :submap)
      (u/assocf compute-svg :submap :svg)
      (u/assocf #(u/fmap % build-js-data) :submap :json)))
  
(defn render-json-svg [{:keys [json svg]}]
  (json/encode
   {:svg (:value svg)
    :json (:value json)}))

(defn render-whole-page [data]
  (html/layout (html/search-page-html data)))

(defroutes app-routes
  (GET "/" [query n-topics]
       (println "Request:" query n-topics)
       (println (session/get query))
       (-> {:query query :n-topics n-topics}
           validate
           prepare-data
           (doto (#(session/put! :session-data %)))
           ;; cache-data!
           render-whole-page))
  (POST "/refine" [query n-topics]
        (println query n-topics)
        (-> {:query query :n-topics n-topics}
            validate
            prepare-data
           (doto (#(session/put! :session-data %)))
            render-json-svg))
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (-> (handler/site app-routes)
      (session/wrap-noir-session)))
