(ns wiki-api.core
  (:require [utils.core :as u]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Protocols and data types that MUST be used by Wikipedia service wrappers ;;;;;;;;;;;;;;;;

(defprotocol IDocument
  (doc-string [this]))

(extend-protocol IDocument
  String
  (doc-string [this] this))

(defrecord Article [id title])

(defrecord Category [id title])

(defrecord DocArticleLink [doc article fragment strength])

(defrecord ArticleRel [articles strength])

(defprotocol IWikiService
  (-annotate [this docs])
  (-relatedness [this article-pairs])
  (-article-categories [this article])
  (-cat-relations [this categories]))

(defn annotate [service & docs]
  (-annotate service docs))

(defn relatedness [service & article-pairs]
  (-relatedness service article-pairs))

(def article-categories -article-categories)

(defn cat-relations [service & categories]
  (-cat-relations service categories))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utility functions ;;;;;;;;;;;;;;;;

(defn select-max-strength [links]
  (let [sort-fn #(sort-by (comp - :strength) %)]
    (->> links
      (group-by (juxt :article :doc))
      (u/map-val sort-fn)
      (u/map-val first)
      vals)))
