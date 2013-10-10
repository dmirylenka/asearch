(ns arxiv-api.core 
  (:require [clojure.data.xml :as xml]
            [clojure.zip :as zip :refer [xml-zip]]
            [clojure.data.zip.xml :as zx :refer [xml-> xml1->]]
            [clojure.java.io :as io]
            [clj-http.client :as http]
            [utils.core :as u])
  (:import (java.io IOException InterruptedIOException)
           (org.apache.http.conn ConnectTimeoutException)))

;TODO: move to configuration
(def arxiv-search-url "http://export.arxiv.org/api/query")

;?search_query=all:statistical%20relational%20learning&start=0&max_results=100")

(def default-params
  {:start 0
   :max_results 10})

(def param-mapping
  {:query :search_query
   :start :start
   :end :max_results})

(def default-timeout 10000)

(defn send-http-request [query-params timeout]
  (try
    (u/->Success
      (http/get arxiv-search-url
                {:as :xml
                 :throw-exceptions false
                 :conn-timeout timeout
                 :socket-timeout timeout
                 :query-params (merge default-params (u/map-key param-mapping query-params))}))
    (catch InterruptedIOException e
      (println "Timeout while querying arXiv:" (.getMessage e))
      (u/->Fail :timeout))
    (catch IOException e
      (println "Exception while querying arXiv:" (.getMessage e))
      (u/->Fail :unavailable))))


(defn get-arxiv-response [http-response]
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

(defn parse-author [author-xml]
  (let [zipper (xml-zip author-xml)]
    {:full-name (xml1-> zipper :name zx/text)}))

(defn parse-paper [paper-xml]
  (let [zipper (xml-zip paper-xml)]
    {:title (xml1-> zipper :title zx/text)
     :id (xml1-> zipper :id zx/text)
     :abstract (xml1-> zipper :summary zx/text)
     :author (map parse-author (xml-> zipper :author zip/node))}))

(defn parse-arxiv-response [xml]
  (let [zipper (xml-zip xml)
        papers (xml-> zipper :entry zip/node)]
    (map parse-paper papers)))

(defn get-papers [arxiv-response]
  ;; (case (:result-code arxiv-response)
    ;; 0
    (u/->Success ; (:result (:publication
                           (parse-arxiv-response (xml/parse-str arxiv-response))
                           ;))
                           )
    ;; 1 (u/->Fail :forbidden)
    ;; 2 (u/->Fail :bad-request)
    ;; 3 (u/->Fail :unavailable)
    ;; 4 (u/->Fail :bad-request)
    ;; (u/->Fail (str "Unexpected result code from MsAcademic: " (:result-code mas-response)))))
)

(defn fail-if-empty [search-results]
  (if (empty? search-results)
    (u/->Fail :empty-results)
    (u/->Success search-results)))

(defn search-papers [query & more]
  (let [opt (apply hash-map more)
        timeout (or (:timeout opt) default-timeout)
        fail-empty? (if (contains? opt :fail-empty)
                      (:fail-empty opt)
                      true)
        query-params (-> opt (assoc :query query)
                       (select-keys (keys param-mapping)))]
    (u/bind (send-http-request query-params timeout)
            get-arxiv-response
            get-papers
            (if fail-empty? fail-if-empty u/->Success))))
