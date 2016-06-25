(ns arnetminer.core 
  (:require [clj-http.client :as http]
            [utils.core :as u]
            [search-api.search-api :as sapi]
            [clojure.set])
  (:import (java.io IOException InterruptedIOException)
           (org.apache.http.conn ConnectTimeoutException)))

;TODO: move to configuration
(def aminer-search-url "https://api.aminer.org/api/search/pub")

(def default-params
  {:offset 0
   :sort "relevance"
   :size 100})

(def param-mapping
  {:query :query
   :end :size})

(def default-timeout 10000)

(defn send-http-request [query-params timeout]
  (try
    (u/->Success
      (http/get aminer-search-url
                {:as :json
                 :throw-exceptions false
                 :conn-timeout timeout
                 :socket-timeout timeout
                 :query-params (merge default-params (u/map-key param-mapping query-params))}))
    (catch InterruptedIOException e
      (println "Timeout while querying Arnetminer:" (.getMessage e))
      (u/->Fail :timeout))
    (catch IOException e
      (println "Exception while querying Arnetminer:" (.getMessage e))
      (u/->Fail :unavailable))))


(defn get-aminer-response [http-response]
  (try
    (let [{:keys [status body]} http-response]
      (if (= 200 status)
        (u/->Success (u/keys-to-dash body))
        (do
          (println "HTTP request failed: " status "; " http-response)
          (u/->Fail :unavailable))))
    (catch ConnectTimeoutException e
      (.printStackTrace e)
      (u/->Fail :timeout))))

(defn get-papers [aminer-response]
  (u/->Success (:result aminer-response)))

(defn fail-if-empty [search-results]
  (if (empty? search-results)
    (u/->Fail :empty-results)
    (u/->Success search-results)))

(defn mk-author [id name]
  {:id id :full-name name})

(defn extract-authors [paper]
  (let [ids (:author-ids paper)
        names (map :name (:authors paper))
        authors (map mk-author ids names)]
    (-> paper
        (dissoc :authors :author-ids)
        (assoc :author authors))))

(defn mk-paper [paper]
  (-> paper
      (clojure.set/rename-keys {:abstract :abstract
                                :doi :key ;; What is this?
                                :num_citation :ncit})
      extract-authors
      (select-keys sapi/paper-fields)
      (update-in [:author] (partial map #(select-keys % sapi/author-fields)))
      sapi/mk-paper))

(defn search-papers [query & more]
  (let [opt (apply hash-map more)
        timeout (or (:timeout opt) default-timeout)
        fail-empty? (if (contains? opt :fail-empty)
                      (:fail-empty opt)
                      true)
        query-params (-> opt (assoc :query query)
                       (select-keys (keys param-mapping)))]
    (u/bind (send-http-request query-params timeout)
            get-aminer-response
            get-papers
            (if fail-empty? fail-if-empty u/->Success)
            (comp u/->Success #(map mk-paper %)))))

(deftype AcademicSearch [init-params]
  sapi/IAcademicSearch
  (-search-papers [this query params]
    (apply search-papers query
           (apply concat (merge init-params params)))))

(defn service [& {:as params}]
  (->AcademicSearch params))
