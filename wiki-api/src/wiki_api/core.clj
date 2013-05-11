(ns wiki-api.core
  (:require [utils.core :as u]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Protocols that MUST be implemented by Wikipedia service wrappers ;;;;;;;;;;;;;;;;

(defprotocol IDocument
  (doc-string [this]))

(extend-protocol IDocument
  String
  (doc-string [this] this))

(defprotocol IArticle
  (article-title [this])
  (article-id [this]))

(defprotocol ICategory
  (category-title [this])
  (category-id [this]))

(defprotocol IDocArticleLink
  (link-doc [this])
  (link-article [this])
  (link-fragment [this])
  (link-strength [this]))

(defprotocol IArticleRel
  (rel-articles [this])
  (rel-strength [this]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Records that MAY be used by Wikipedia service wrappers ;;;;;;;;;;;;;;;;

(defrecord Article [id title]
  IArticle
  (article-title [this] title)
  (article-id [this] id))

(defrecord Category [id title]
  ICategory 
  (category-title [this] title)
  (category-id [this] id))

(defrecord DocArticleLink [doc article fragment strength]
  IDocArticleLink
  (link-doc [this] doc)
  (link-article [this] article)
  (link-fragment [this] fragment)
  (link-strength [this] strength))

(defrecord ArticleRel [articles strength]
  IArticleRel
  (rel-articles [this] articles)
  (rel-strength [this] strength))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utility functions ;;;;;;;;;;;;;;;;

(defn select-max-strength [links]
  (let [sort-fn #(sort-by (comp - link-strength) %)]
    (->> links
      (group-by (juxt link-article link-doc))
      (u/map-val sort-fn)
      (u/map-val first)
      vals)))

