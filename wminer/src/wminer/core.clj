(ns wminer.core
  (:import [org.wikipedia.miner.util WikipediaConfiguration]
           [org.wikipedia.miner.model Wikipedia Article Category Redirect Page Page$PageType]
           [org.wikipedia.miner.annotation Disambiguator TopicDetector Topic]
           [org.wikipedia.miner.annotation.weighting LinkDetector]
           [org.wikipedia.miner.comparison ArticleComparer])
  (:require (clojure.java [io :as io])
            (utils [core :as u]
                   [text :as t])
            (wiki-api [core :as wapi])
            (clojure [string :as string])))

(def resources
  (delay 
    (let [wiki-conf (WikipediaConfiguration. (io/file "/Users/dmirylenka/code/asearch-modular/wminer/resources/wikipedia-template.xml")) 
          wikipedia (Wikipedia. wiki-conf false)
          disambiguator (Disambiguator. wikipedia)
          topic-detector (TopicDetector. wikipedia disambiguator true false)
          link-detector (LinkDetector. wikipedia)
          art-comparer (ArticleComparer. wikipedia)]
      {:wikipedia wikipedia
       :topic-detector topic-detector
       :link-detector link-detector
       :art-comparer art-comparer})))

(defn ^Wikipedia wikipedia []
  (@resources :wikipedia))

(defn ^TopicDetector topic-detector []
  (@resources :topic-detector))

(defn ^LinkDetector link-detector []
  (@resources :link-detector))

(defn ^ArticleComparer art-comparer []
  (@resources :art-comparer))

(defn page-by-id [id]
  (.getPageById (@resources :wikipedia) id))

(defn resolve-redirect [^Article article]
  (let [^Redirect redirect (page-by-id (.getId article))
        target (.getTarget redirect)]
    (when-not target
      (println "Empty target page, probably redirect loop:" article))
    target))

(defn redirect? [^Page page]
  (= Page$PageType/redirect (.getType page)))

;; (defn resolve-if-redirect [^Topic topic]
;;   (cond-> topic
;;     (redirect? topic) resolve-redirect))

(defn mk-wapi-article [id]
  (let [page (page-by-id id)
        article (cond-> page (redirect? page) resolve-redirect)]
    (when article
      (wapi/->Article (.getId article) (.getTitle article)))))

;; (defn ->wapi-article [^Topic topic] 
;;   (let [article (resolve-if-redirect topic)]
;;     (when article
;;       (wapi/->Article (.getId article) (.getTitle article)))))

(defn ->wapi-article [^Topic topic] 
  (mk-wapi-article (.getId topic) (.getTitle topic)))

(defn ^Article ->wminer-article [article]
  (.getPageById (wikipedia) (:id article)))

(defn ->wapi-category [^Category category]
  (wapi/->Category (.getId category) (.getTitle category)))

(defn ^Category ->wminer-category [category]
  (.getPageById (wikipedia) (:id category)))

(defn get-wiki-topics
  "Returns a collection of wikiminer Topics detected in a string."
  [^String string & [probability]]
  (let [all-topics (.getTopics (topic-detector) string nil)]
    (if-not probability
      all-topics
      (.getBestTopics (link-detector) all-topics probability))))

(defn get-wiki-topics-jointly
  "Returns a collection of wikiminer Topics detected in a string."
  [snippets & [probability]]
  (println "joint annotation")
  (let [snippets (map #(str %". ") snippets)
        query (string/join snippets)
        snippet-ends (reductions + (map count snippets))
        all-topics (.getTopics (topic-detector) query nil)
        topics (if-not probability
                 all-topics
                 (.getBestTopics (link-detector) all-topics probability))
        topic-pos (sort-by (comp (memfn getEnd) second)
                           (for [topic topics
                                 pos (.getPositions topic)]
                             [topic pos]))
        iter (fn [topics-grouped topic-pos-left snippet-ends]
               (if (empty? snippet-ends)
                 topics-grouped
                 (let [next-position (first snippet-ends)
                       split-fn #(< (.. (second %) (getEnd)) next-position)
                       [topics-before topics-after] (split-with split-fn topic-pos-left)]
                   (recur (conj topics-grouped (set (map first topics-before))) topics-after (rest snippet-ends)))))]
(iter [] topic-pos snippet-ends)))

(defn- get-title [^Topic topic]
  (.getTitle topic))

(defn get-topics
  "Same as get-wik-topics, but returns only topic titles in the format as in the URLs of Wikipedia articles."
  [string & [probability]]
  (map get-title (get-wiki-topics string probability)))

(defn article-by-title [title]
  (.getArticleByTitle (@resources :wikipedia) title))

(defn out-links [title]
  (map (memfn getTitle)
       (.getLinksOut (article-by-title title))))

(defn cat-by-title [title]
  (.getCategoryByTitle (@resources :wikipedia) title))

(def page-types
  {:article Page$PageType/article
   :category Page$PageType/category})

(defn topic-out-links
  "If topic is a page gets its out-links in Wikipedia, if it is a category, get the out-links of its main page."
  [topic-title]
  (let [article-candidate (article-by-title topic-title)
        stem (comp t/stem t/string->words)
        main-article-by-cat (fn [category-title]
                              (let [category (cat-by-title category-title)
                                    cat-articles (.getChildArticles category)
                                    stemmed-title (stem topic-title)
                                    main-article? #(= stemmed-title (stem (.getTitle %)))]
                                (first (filter main-article? cat-articles))))

        article (or article-candidate (main-article-by-cat topic-title))]
    (if article
      (->> article .getLinksOut (map (memfn getTitle)) #_(map string/lower-case))
      (throw (Exception. (str "Could not find neither article nor category with the main article:" topic-title))))))

(defn parent-cats [cat]
  (let [parents (map ->wapi-category
                     (.getParentCategories
                      (->wminer-category cat)))
        freqs (frequencies parents)
        non-unique (map first (filter #(> (second %) 1) freqs))]
    (doseq [parent non-unique]
      (println "Weirdly, parent" parent "occurs more than once for" cat))
    (set parents)))

(deftype WikiService []
  wapi/IWikiService
  (-annotate [this docs]
    (let [->wapi-article-cached (comp (memoize mk-wapi-article) (memfn getId))
          doc-strings (map wapi/doc-string docs)
          ;joint-string (string/join ". " doc-strings)
          ;doc-topics (map vector docs (get-wiki-topics-jointly doc-strings 5e-1))
          ]
      (for [doc docs 
            ;[doc topics] doc-topics
            :let [doc-string (wapi/doc-string doc)]
            topic (get-wiki-topics doc-string 5e-1)
            ;topic topics
            :let [wapi-article (->wapi-article-cached topic)]
            :when wapi-article
            position (.getPositions topic)]
        (wapi/->DocArticleLink doc wapi-article
                               (.substring doc-string (.getStart position) (.getEnd position))
                               (.getWeight topic)))))
  (-relatedness [this article-pairs]
    (for [[a1 a2] article-pairs
          :let [wminer-a1 (->wminer-article a1)
                wminer-a2 (->wminer-article a2)
                score (.getRelatedness (art-comparer) wminer-a1 wminer-a2)]]
      (wapi/->ArticleRel #{a1 a2} score)))
  (-article-categories [this article]
    (map ->wapi-category (.getParentCategories (->wminer-article article))))
  (-cat-relations [this categories]
    (let [wiki (wikipedia)
          categories (set categories)]
      (for [cat categories
            parent (filter categories (parent-cats cat))]
        [cat parent]))))

(def service (WikiService.))
