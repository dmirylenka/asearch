(ns mas-api.core 
  (:require [clj-http.client :as http]
            [utils.core :as u])
  (:import (java.io IOException InterruptedIOException)
           (org.apache.http.conn ConnectTimeoutException)))

;TODO: move to configuration
(def mas-search-url "http://academic.research.microsoft.com/json.svc/search")

;TODO: move to configuration
(def ^:private ms-app-id "ec26a381-a89f-4749-b946-78d95175982d")

(def default-params
  {:AppId ms-app-id
   :ResultObjects "Publication"
   :PublicationContent "AllInfo"
   :StartIdx 1
   :EndIdx 10})

(def param-mapping
  {:app-id :AppId
   :query :FullTextQuery
   :object :ResultObjects
   :content :PublicationContent
   :start :StartIdx
   :end :EndIdx})

(def default-timeout 10000)

(defn send-http-request [query-params timeout]
  (try
    (u/->Success
      (http/get mas-search-url
                {:as :json
                 :throw-exceptions false
                 :conn-timeout timeout
                 :socket-timeout timeout
                 :query-params (merge default-params (u/map-key param-mapping query-params))}))
    (catch InterruptedIOException e
      (println "Timeout while querying MsAcademic:" (.getMessage e))
      (u/->Fail :timeout))
    (catch IOException e
      (println "Exception while querying MsAcademic:" (.getMessage e))
      (u/->Fail :unavailable))))

(defn get-mas-response [http-response]
  (try
    (let [{:keys [status body]} http-response]
      (if (= 200 status)
        (u/->Success (:d (u/keys-to-dash body)))
        (do
          (println "HTTP request failed: " status "; " http-response)
          (u/->Fail :unavailable))))
    (catch ConnectTimeoutException e
      (.printStackTrace e)
      (u/->Fail :timeout))))

(defn get-papers [mas-response]
  (case (:result-code mas-response)
    0 (u/->Success (:result (:publication mas-response)))
    1 (u/->Fail :forbidden)
    2 (u/->Fail :bad-request)
    3 (u/->Fail :unavailable)
    4 (u/->Fail :bad-request)
    (u/->Fail (str "Unexpected result code from MsAcademic: " (:result-code mas-response)))))

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
            get-mas-response
            get-papers
            (if fail-empty? fail-if-empty u/->Success))))
