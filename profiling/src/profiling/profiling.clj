(ns profiling.profiling
  (:require [mas-api.core :as mas]
            [wiki-api.core :as wapi]
            [wminer.core :as wminer]))

(def results (:value (mas/search-papers "clustering methods" :end 100)))

(def strings (mapv #(str (:title %) ". " (:abstract %)) results))

;(wapi/annotate wminer/service "statistical relational learning")
