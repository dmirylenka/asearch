(ns profiling.profiling
  (:require [clojure.string :as string]
            [search-api.search-api :as sapi]
            [mas-api.core :as mas]
            [arxiv-api.core :as arxiv]
            [arnetminer.dataset :as aminer]
            [wiki-api.core :as wapi]
            [wminer.core :as wminer]
            [wminer-api.core :as wminer-api]
            [mlinking.core :as mlinking]
            [topic-maps.core :as tmaps]))

(defn mk-paper [{:keys [id author title abstract year] :as paper}]
  (let [doc-id (str title " " year " " (doall (string/join ", " (map :last-name author))))
        doc-string (str title ". " abstract)]
    (-> paper
        (assoc :doc-id doc-id
               :string doc-string))))

(defn distinct-docs [docs]
  (->> docs (group-by :doc-id) vals (map first)))

(def results
  (->> (sapi/search-papers aminer/service "clustering methods" :end 10)
       :value
       (map mk-paper)))

(def doc-map (zipmap (map :id results) results))

(def doc-strings (mapv :string (vals doc-map)))

(def strings (mapv :string results))

#_(wapi/annotate wminer/service "statistical relational learning")

