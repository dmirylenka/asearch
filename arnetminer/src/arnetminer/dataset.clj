(ns arnetminer.dataset
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split blank?]]
            [clojure.java.jdbc :as jdbc]
            [clojure.java.jdbc.sql :as sql]
            [search-api.search-api :as sapi]
            [arnetminer.core :as aapi]
            [utils.core :as u]))

(def db {:subprotocol "mysql"
         :subname "//localhost:3306/aminer_dataset"
         :user "root"
         :password ""})

(defn paper-by-key [id]
  (let [results (jdbc/query db
                  (sql/select *
                    :aminer_papers
                    (sql/where {:id id})))]
    (if (next results)
      (throw (Exception. (str "More than one paper with id: " id)))
      (first results))))

(defn augment-with-abstract [paper]
  (assoc paper :abstract (:abstract (paper-by-key (:id paper)))))

(deftype AcademicSearch [delegate]
  sapi/IAcademicSearch
  (-search-papers [this query params]
    (u/fmap (sapi/-search-papers delegate query params)
            #(mapv augment-with-abstract %))))

(def service (->AcademicSearch (aapi/->AcademicSearch)))
