(ns arnetminer.dataset
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split blank?]]
            [clojure.java.jdbc :as jdbc]
            [clojure.java.jdbc.sql :as sql]
            [search-api.search-api :as sapi]
            [arnetminer.core :as aapi]
            [utils.core :as u]))

(def default-db
  "An example of the DB configuration."
  {:subprotocol "mysql"
   :subname "//localhost:3306/aminer_dataset"
   :user "root"
   :password ""})

(defn paper-by-key [db id]
  (let [results (jdbc/query db
                  (sql/select *
                    :aminer_papers
                    (sql/where {:id id})))]
    (if (next results)
      (throw (Exception. (str "More than one paper with id: " id)))
      (first results))))

(defn augment-with-abstract [db paper]
  (assoc paper :abstract (:abstract (paper-by-key db (:id paper)))))

(deftype AcademicSearch [db delegate]
  sapi/IAcademicSearch
  (-search-papers [this query params]
    (try
      (u/fmap (sapi/-search-papers delegate query params)
              #(mapv (partial augment-with-abstract db) %))
      (catch Exception e
        (.printStackTrace e)
        (u/->Fail (.getMessage e))))))
  
(defn -service [params]
  (->AcademicSearch
   (merge default-db (:db params))
   (aapi/->AcademicSearch (:dissoc :db params))))

(defn service [& {:as params}]
  (-service params))
