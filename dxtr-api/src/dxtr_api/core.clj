(ns dxtr-api.core
  (:require (clojure.java [io :as io])
            (clojure [string :as string])
            [clj-http.client :as http]
            (utils [core :as u]
                   [text :as t])
            (wiki-api [core :as wapi]))
  (:import (java.io IOException InterruptedIOException)
           (org.apache.http.conn ConnectTimeoutException)))
            

;TODO: move to configuration
(def dxtr-url "http://localhost:8080/rest/")

(def default-params {})

(def param-mapping {:query :text
                    :id :id
                    :wid :wid
                    :title-only :title-only})
   
(def default-timeout 30000)

(defn send-http-request [service query-params timeout]
  (try
    (u/->Success
      (http/get (str dxtr-url service)
                {:as :json
                 :throw-exceptions false
                 :conn-timeout timeout
                 :socket-timeout timeout
                 :query-params (merge default-params (u/map-key param-mapping query-params))}))
    (catch InterruptedIOException e
      (println "Timeout while querying Dexter:" (.getMessage e))
      (u/->Fail :timeout))
    (catch IOException e
      (println "Exception while querying Dexter:" (.getMessage e))
      (u/->Fail :unavailable))))

(defn get-dxtr-response [http-response]
  (try
    (let [{:keys [status body]} http-response]
      (if (= 200 status)
        (u/->Success body)
        (do
          (println "HTTP request failed: " status "; " http-response)
          (u/->Fail :unavailable))))
    (catch ConnectTimeoutException e
      (.printStackTrace e)
      (u/->Fail :timeout))))

(defn get-annotations [dxtr-response]
  (u/->Success (:spots dxtr-response)))

(defn get-description [dxtr-response]
  (u/->Success dxtr-response))

(defn get-categories [dxtr-response]
  (u/->Success dxtr-response))

(defn title-by-id
  "Fetches the title of the Wikipedia page."
  [id & more]
  (let [opt (apply hash-map more)
        timeout (or (:timeout opt) default-timeout)
        query-params {:id id
                      :title-only true}]
    (u/bind (send-http-request "get-desc" query-params timeout)
            get-dxtr-response
            get-description
            :title)))

(defn get-article-cat-ids
  "Fetches the ids of the article's categories given the article's id."
  [id & more]
  (let [opt (apply hash-map more)
        timeout (or (:timeout opt) default-timeout)
        query-params {:wid id}]
    (:value (u/bind (send-http-request "graph/get-entity-categories" query-params timeout)
                    get-dxtr-response
                    get-categories))))

(defn get-article-cats
  "Fetches the article's categories given the article's id."
  [id & more]
  (let [ids (apply get-article-cat-ids id more)]
    (doall
     (for [id ids]
       {:id id
        :title (subs (title-by-id id) (count "Category:"))}))))

(defn update-with-title
  "Takes the id of the article, and updates the article with the title retrieved from Wikipedia by this id."
  [{:keys [entity] :as article}]
  (assoc article :title (title-by-id entity)))

(defn article-cat-ids [article]
  (get-article-cat-ids (:id article)))

(defn article-cats [article]
  (get-article-cats (:id article)))

(defn get-wiki-topics
  "Returns a collection of Wikipedia topics, detected in a string (in the Dexter format)."
  [snippet & more]
  (let [opt (apply hash-map more)
        timeout (or (:timeout opt) default-timeout)
        query-params (-> opt (assoc :query snippet)
                       (select-keys (keys param-mapping)))]
     (u/bind (send-http-request "annotate" query-params timeout)
             get-dxtr-response
             get-annotations
             (partial map update-with-title))))

(defn get-parent-cat-ids
  "Fetches the ids of the parent categories given the category id."
  [id & more]
  (let [opt (apply hash-map more)
        timeout (or (:timeout opt) default-timeout)
        query-params {:wid id}]
    (:value (u/bind (send-http-request "graph/get-parent-categories" query-params timeout)
                    get-dxtr-response
                    get-categories))))

(defn get-parent-cats
  "Fetches the parent categories given the category id."
  [id & more]
  (let [ids (apply get-parent-cat-ids id more)]
    (doall
     (for [id ids]
       {:id id
        :title (subs (title-by-id id) (count "Category:"))}))))

(defn parent-cat-ids [cat]
  (get-parent-cat-ids (:id cat)))

(defn parent-cats [cat]
  (get-parent-cats (:id cat)))

(defn ->wapi-article [article]
  (wapi/mk-article (:entity article) (:title article)))

(defn ->wapi-category [cat]
  (wapi/mk-article (:id cat) (:title cat)))

(defn cat-relations [cats]
  (let [cats-by-id (u/key-map :id cats)]
    (doall
     (for [cat cats
           parent (keep cats-by-id (parent-cat-ids cat))]
       [cat parent]))))

;; (defn topic-out-links
;;   "If topic is a page gets its out-links in Wikipedia, if it is a category, get the out-links of its main page."
;;   [topic-title]
;;   (let [article-candidate (article-by-title topic-title)
;;         stem (comp t/stem t/string->words)
;;         main-article-by-cat (fn [category-title]
;;                               (let [category (cat-by-title category-title)
;;                                     cat-articles (.getChildArticles category)
;;                                     stemmed-title (stem topic-title)
;;                                     main-article? #(= stemmed-title (stem (get-title %)))]
;;                                 (first (filter main-article? cat-articles))))

;;         ^Article article (or article-candidate (main-article-by-cat topic-title))]
;;     (if article
;;       (->> article .getLinksOut (map get-title) #_(map string/lower-case))
;;       (throw (Exception. (str "Could not find neither article nor category with the main article:" topic-title))))))

(deftype WikiService []
  wapi/IWikiService
  (-annotate [this strings]
    (wapi/-annotate this strings nil))
  (-annotate [this strings prob]
    (let [joint-string (string/join ". " strings)
          topic-lists (map get-wiki-topics strings)
          article-links (fn [topic]
                         (->
                          (wapi/->ArticleLink (->wapi-article topic)
                                              (:mention topic)
                                              (:score topic))
                          (assoc :start (:start topic) :end (:end topic))))]
      (mapv (partial mapv article-links) topic-lists)))
  (-article-categories [this article]
    (mapv ->wapi-category (get-article-cats (:id article))))
  (-cat-relations [this categories]
    (cat-relations categories)))

(def service (WikiService.))
