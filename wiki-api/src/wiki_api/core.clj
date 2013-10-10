(ns wiki-api.core
  (:require [utils.core :as u]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Protocols and data types that MUST be used by Wikipedia service wrappers ;;;;;;;;;;;;;;;;

;; evil. rewrite the services and apis to work on strings
;; (defprotocol IDocument
;;   (doc-string [this]))

;; (extend-protocol IDocument
;;   String
;;   (doc-string [this] this))

;; (defrecord Article [id title])

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

;; (defrecord Category [id title])

(defrecord ArticleLink [article fragment strength])

;; (defrecord ArticleRel [articles strength])

(defprotocol IWikiService
  (-annotate [this strings] [this strings prob])
  (-relatedness [this article-pairs])
  (-article-categories [this article])
  (-cat-relations [this categories])
  (-search [this string opt]))

(defn annotate [service & docs]
  (-annotate service docs))

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
