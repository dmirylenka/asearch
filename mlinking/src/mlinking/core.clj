(ns mlinking.core
  (:require (clojure.java [io :as io])
            (clojure [string :as string])
            [clj-http.client :as http]
            (utils [core :as u]
                   [text :as t])
            (wiki-api [core :as wapi])
            [wminer.core :as wminer])
  (:import (java.io IOException InterruptedIOException)
           (org.apache.http.conn ConnectTimeoutException)))
            

;TODO: move to configuration
(def mlinking-url "http://api.machinelinking.com/annotate")

;TODO: move to configuration
(def ^:private mlinking-app-id "")

;TODO: move to configuration
(def ^:private mlinking-app-key "")

(def default-params
  {:app_id mlinking-app-id
   :app_key mlinking-app-key
   :lang "en"
   :min_weight 0.25
   :disambiguation 1
   :category 1
   :output_format "json"
   :include_text 0
   :class 0})

(def param-mapping
  {:app-id :app-id
   :query :text
   :prob :min_weight})

(def default-timeout 10000)

(defn send-http-request [query-params timeout]
  (try
    (u/->Success
      (http/get mlinking-url
                {:as :json
                 :throw-exceptions false
                 :conn-timeout timeout
                 :socket-timeout timeout
                 :query-params (merge default-params (u/map-key param-mapping query-params))}))
    (catch InterruptedIOException e
      (println "Timeout while querying Machine Linking:" (.getMessage e))
      (u/->Fail :timeout))
    (catch IOException e
      (println "Exception while querying Machine Linking:" (.getMessage e))
      (u/->Fail :unavailable))))

(defn get-mlinking-response [http-response]
  (try
    (let [{:keys [status body]} http-response]
      (if (= 200 status)
        (u/->Success ;(u/keys-to-dash body)
         body)
        (do
          (println "HTTP request failed: " status "; " http-response)
          (u/->Fail :unavailable))))
    (catch ConnectTimeoutException e
      (.printStackTrace e)
      (u/->Fail :timeout))))

(defn get-annotations [mlinking-response]
  (u/->Success (:keyword (:annotation mlinking-response))))

(defn get-wiki-topics
  "Returns a collection of Wikipedia topics, detected in a string (in the Machine Linking format)."
  [snippet & more]
  (let [opt (apply hash-map more)
        timeout (or (:timeout opt) default-timeout)
        query-params (-> opt (assoc :query snippet)
                       (select-keys (keys param-mapping)))]
    (:value
     (u/bind (send-http-request query-params timeout)
             get-mlinking-response
             get-annotations))))

(defn get-wiki-topics-jointly
  "Returns a collection of Wikipedia topics (in the Machine Linking format) detected in a collection of strings."
  [snippets & [prob]]
  (println "joint annotation")
  (let [snippets (map #(str % ". ") snippets)
        query (string/join snippets)
        snippet-ends (reductions + (map count snippets))
        topics (get-wiki-topics query :prob prob)
        get-end (comp :end :span)
        topic-pos (sort-by (comp get-end second)
                           (for [topic topics
                                 pos (:ngram topic)]
                             [topic pos]))
        iter (fn [topics-grouped topic-pos-left snippet-ends]
               (if (empty? snippet-ends)
                 topics-grouped
                 (let [next-position (first snippet-ends)
                       split-fn #(< (-> % second get-end) next-position)
                       [topics-before topics-after] (split-with split-fn topic-pos-left)]
                   (recur (conj topics-grouped topics-before) topics-after (rest snippet-ends)))))]
    (iter [] topic-pos snippet-ends)))

(defn get-wiki-topics-chunked
  [snippets chunk-size & [prob]]
  {:pre [(<= 1 chunk-size)]}
  (println "chunked annotation")
  (let [chunks (partition-all chunk-size snippets)]
    (doall (mapcat #(get-wiki-topics-jointly % prob) chunks))))

;; (defn ->wapi-category [cat]
;;   (let [title (:label cat)
;;         wminer-cat (wminer/cat-by-title title)]
;;     (when wminer-cat
;;       (wminer/->wapi-category wminer-cat))))

(defn ->wapi-category [cat]
  (wapi/mk-category (:label cat) (:label cat)))

;; (defn ->wapi-article [topic]
;;   (when (nil? (:page (:sense topic)))
;;     (println "Nil topic: " topic))
;;   (let [cats (:category topic)
;;         title (string/replace (:page (:sense topic)) #"_" " ")
;;         wminer-article (wminer/article-by-title title)
;;         wiki-cats (doall (keep ->wapi-category cats))]
;;     (when wminer-article
;;       (-> (wminer/->wapi-article wminer-article)
;;           (assoc :cats (doall (keep ->wapi-category cats)))))))

(defn ->wapi-article [topic]
  (when (nil? (:page (:sense topic)))
    (println "Nil topic: " topic))
  (let [cats (:category topic)
        title (string/replace (:page (:sense topic)) #"_" " ")
        wiki-cats (map ->wapi-category cats)]
    (-> (wapi/mk-article title title)
        (assoc :cats wiki-cats))))

(defn parent-cats [cat]
  (when-let [wminer-cat (wminer/cat-by-title (:title cat))]
    (let [wminer-parents (.getParentCategories wminer-cat)
          mk-category #(let [title (.getTitle %)] (wapi/mk-category title title))]
      (mapv mk-category wminer-parents))))

(defn cat-relations [cats]
  (let [cats (set cats)]
    (for [cat cats
          parent (distinct (filter cats (parent-cats cat)))]
      [cat parent])))

(deftype WikiService []
  wapi/IWikiService
  (-annotate [this strings]
    (wapi/-annotate this strings 0.3))
  (-annotate [this strings prob]
    (let [joint-string (string/join ". " strings)
          doc-topics (map vector strings (get-wiki-topics-chunked strings 5 prob))
          article-link (fn [[topic position]]
                         (when (:sense topic)
                           (let [wapi-article (->wapi-article topic)
                                 span (:span position)
                                 start (:start span)
                                 end (:end span)]
                             (when wapi-article
                               (->
                                (wapi/->ArticleLink wapi-article
                                                    (.substring joint-string start end)
                                                    (:prob (:sense topic)))
                                (assoc :start start :end end))))))]
      (doall
       (for [[string topic-pos] doc-topics]
         (doall (keep article-link topic-pos))))))
  (-article-categories [this article]
    (:cats article))
  (-cat-relations [this categories]
    (cat-relations categories))
    #_(wapi/-cat-relations wminer/service categories))


(def service (WikiService.))
