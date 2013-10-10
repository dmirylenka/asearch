(ns search-api.search-api)

(defrecord Author [id first-name last-name full-name])

(defrecord Paper [id key title abstract author year venue ncit])

(defn mk-author [author]
  (map->Author author))

(defn mk-paper [paper]
  (-> (map->Paper paper)
      (update-in [:author] (partial map mk-author))))

(def paper-fields (keys (mk-paper {})))

(def author-fields (keys (mk-author {})))

(def default-timeout 10000)

(defprotocol IAcademicSearch
  (-search-papers [this query params]))

;; params: end, timeout
(defn search-papers [service query & {:as params}]
  (-search-papers service query params))
