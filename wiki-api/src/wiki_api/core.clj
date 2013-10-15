(ns wiki-api.core
  (:require [utils.core :as u]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Protocols and data types that MUST be used by Wikipedia service wrappers ;;;;;;;;;;;;;;;;

(defn mk-article [id title]
  {:id id
   :title title
   :type ::article})

(defn mk-category [id title]
  {:id id
   :title title
   :type ::category})

(def article? (comp (partial = ::article) :type))

(def category? (comp (partial = ::category) :type))

(defrecord ArticleLink [article fragment strength])

(defprotocol IWikiService
  (-annotate [this strings] [this strings prob])
  (-relatedness [this article-pairs])
  (-article-categories [this article])
  (-cat-relations [this categories])
  (-search [this string opt]))

(defn annotate [service & docs]
  (let [link-seqs (-annotate service docs)]
    (doseq [links link-seqs
            link links]
      (when (or (clojure.string/blank? (:fragment link))
                (clojure.string/blank? (:title (:article link))))
        (println "Empty annotation:" link)))
    link-seqs))

(defn relatedness [service & article-pairs]
  (-relatedness service article-pairs))

(def article-categories -article-categories)

(defn cat-relations [service & categories]
  (-cat-relations service categories))

(def search* -search)

(defn search [service query & {:as opt}]
  (search* service query opt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utility functions ;;;;;;;;;;;;;;;;

(defn select-max-strength [links]
  (let [sort-fn #(sort-by (comp - :strength) %)]
    (->> links
      (group-by :article)
      (u/map-val sort-fn)
      (u/map-val first)
      vals
      (sort-by :start))))
