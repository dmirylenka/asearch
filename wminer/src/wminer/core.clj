(ns wminer.core
  (:import [org.wikipedia.miner.util WikipediaConfiguration Position]
           [org.wikipedia.miner.model Wikipedia Article Category Redirect Page Page$PageType Label Label$Sense]
           [org.wikipedia.miner.annotation Disambiguator TopicDetector Topic]
           [org.wikipedia.miner.annotation.weighting LinkDetector]
           [org.wikipedia.miner.comparison ArticleComparer]
           [org.wikipedia.miner.util NGrammer NGrammer$NGramSpan])
  (:require (clojure.java [io :as io])
            (utils [core :as u]
                   [text :as t])
            (wiki-api [core :as wapi])
            (clojure [string :as string])))

(def resources
  (delay 
    (let [wiki-conf (WikipediaConfiguration. (io/input-stream (io/resource "wikipedia-template.xml")))
          wikipedia (Wikipedia. wiki-conf false)
          disambiguator (Disambiguator. wikipedia)
          topic-detector (TopicDetector. wikipedia disambiguator)
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

(defn ^Page page-by-id [id]
  (.getPageById (wikipedia) id))

(defn get-id [^Page page]
  (.getId page))

(defn ^Article resolve-redirect [^Article article]
  (let [^Redirect redirect (page-by-id (.getId article))
        target (.getTarget redirect)]
    (when-not target
      (println "Empty target page:" article))
    target))

(defn redirect? [^Page page]
  (= Page$PageType/redirect (.getType page)))

(defn mk-wapi-article [id]
  (let [page (page-by-id id)
        ^Article article (cond-> page (redirect? page) resolve-redirect)]
    (when article
      (wapi/mk-article (.getId article) (.getTitle article)))))

(defn ->wapi-article [^Topic topic] 
  (mk-wapi-article (.getId topic)))

(defn ^Article ->wminer-article [article]
  (.getPageById (wikipedia) (:id article)))

(defn ->wapi-category [^Category category]
  (wapi/mk-category (.getId category) (.getTitle category)))

(defn ^Category ->wminer-category [category]
  (.getPageById (wikipedia) (:id category)))

(defn get-wiki-topics
  "Returns a collection of wikiminer Topics detected in a collection of strings."
  [^String string & [probability]]
  (let [all-topics (.getTopics (topic-detector) string nil)]
    (if-not probability
      all-topics
      (.getBestTopics (link-detector) all-topics probability))))

(defn get-wiki-topics-jointly
  "Returns a collection of wikiminer Topics detected in a string."
  [snippets & [probability]]
  (let [snippets (map #(str % ". ") snippets)
        query (string/join snippets)
        snippet-ends (reductions + (map count snippets))
        all-topics (.getTopics (topic-detector) query nil)
        topics (if-not probability
                 all-topics
                 (.getBestTopics (link-detector) all-topics probability))
        get-end (fn [^Position pos] (.getEnd pos))
        topic-pos (sort-by (comp get-end second)
                           (for [^Topic topic topics
                                 ^Position pos (.getPositions topic)]
                             [topic pos]))
        iter (fn [topics-grouped topic-pos-left snippet-ends]
               (if (empty? snippet-ends)
                 topics-grouped
                 (let [next-position (first snippet-ends)
                       split-fn #(< (-> % second get-end) next-position)
                       [topics-before topics-after] (split-with split-fn topic-pos-left)]
                   (recur (conj topics-grouped topics-before) topics-after (rest snippet-ends)))))]
(iter [] topic-pos snippet-ends)))

(defn- get-title [^Page page]
  (.getTitle page))

(defn get-prob [^Label$Sense sense]
  (.getPriorProbability sense))

(defn get-topics
  "Same as get-wiki-topics, but returns only topic titles in the format as in the URLs of Wikipedia articles."
  [string & [probability]]
  (map get-title (get-wiki-topics string probability)))

(defn ^Article article-by-title [title]
  (.getArticleByTitle (wikipedia) title))

(defn out-links [title]
  (map get-title
       (.getLinksOut (article-by-title title))))

(defn ^Category cat-by-title [title]
  (.getCategoryByTitle (wikipedia) title))

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
                                    main-article? #(= stemmed-title (stem (get-title %)))]
                                (first (filter main-article? cat-articles))))

        ^Article article (or article-candidate (main-article-by-cat topic-title))]
    (if article
      (->> article .getLinksOut (map get-title) #_(map string/lower-case))
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

(defn get-senses*
  "Gets possible senses (org.wikipedia.miner.model.Label$Sense) for the query.
   Analogous to the search web service of Wikipedia miner with complex=false
   (the whole query rather than its parts should refer to the sence).
   Options:
   - :min-prob : the minimum probability of the sense returned"
  [^String query {:keys [min-prob] :as opt :or {min-prob 0}}]
  (let [wiki (wikipedia)
        ngrammer (NGrammer. (.. wiki (getConfig) (getSentenceDetector)) (.. wiki (getConfig) (getTokenizer)))
        ^NGrammer$NGramSpan span (first (.ngramPosDetect ngrammer query))
         label (.getLabel wiki span query)]
    (->> (.getSenses label)
         (filter #(> (get-prob %) min-prob)))))

(defn get-senses
  "Gets possible senses (org.wikipedia.miner.model.Label$Sense) for the query.
   Analogous to the search web service of Wikipedia Miner with complex=false
   (the whole query rather than its parts should refer to the sence).
   Options:
      :min-prob : the minimum probability of the sense returned."
  [query & {:as opt}]
  (get-senses* query opt))

;; (defn resolve-collisions [article-links]
;;   (letfn [(intersects? [link1 link2]
;;             (not (or (>= (:start link1) (:end link2))
;;                      (>= (:start link2) (:end link1)))))
;;           (split-intersecting [links]
;;             (when-let [fst (first links)]
;;               (split-with #(intersects? fst %) links)))
;;           (iter [result links]
;;             (if (empty? links)
;;               result
;;               (let [[intersecting other] (split-intersecting links)]
;;                 (recur (conj result (apply max-key :strength intersecting))
;;                        other))))]
;;     (iter [] article-links)))

(defn resolve-collisions-as-wminer
  "Takes annotations of text fragments with Wikipedia articles, and filters them so that each text fragment points to at most one article.
   The code is a rewrite of the resolveCollisions method of org.wikipedia.miner.annotation.tagging.DocumentTagger.
   The rule for resolving intersecting annotations is to check if the outermost annotation has bigger weight that the maximum weight of the
   innermost annotations multiplied by 0.8. Depending on the result either the outermost annotation or the collection of inner annotations are kept."
  [article-links]
  (letfn [(intersects? [link1 link2]
            (not (or (>= (:start link1) (:end link2))
                     (>= (:start link2) (:end link1)))))
          (link-report [inner-links outer-link]
            (str (:fragment outer-link) ": "
                 (:title (:article outer-link)) ": "
                 (apply str (map (comp :title :article) inner-links))))
          (iter [result links]
            (if (empty? links)
              result
              (let [outer-link (first links)
                    inner-links (seq (take-while #(intersects? outer-link %) (next links)))
                    max-inner (when inner-links (apply max (map :strength inner-links)))
                    [rest-links keep-links] (if (and max-inner
                                                     (> (* 0.8 max-inner)
                                                        (:strength outer-link)))
                                              (do #_ (println "inner links: " (link-report inner-links outer-link))
                                                  [(next links) []])
                                              (do #_ (when max-inner
                                                    (println "outer link: " (link-report inner-links outer-link)))
                                                   [(drop (count inner-links) (next links)) [outer-link]]))]
                (recur (into result keep-links) rest-links))))]
    (iter [] (sort-by (juxt :start (comp - :end)) article-links))))
        
(defn all-caps? [string]
  (= string (string/upper-case string)))

(defn single-word? [string]
  (= 1 (count (t/string->words string))))

(defn single-noncap? [string]
  (and (single-word? string)
       (not (all-caps? string))))

(defn remove-single-word-links
  "Takes a collection of links from text fragments to Wikipedia articles,
   and removes the links in which the text fragment consists of a single word."
  [links]
  (let [single-word-link? #(single-noncap? (:fragment %))
        link->string #(str "[" (:fragment %) "|" (:title (:article %)) "]")
        single-word-links (filter single-word-link? links)]
    (println (map link->string single-word-links))
    (remove single-word-link? links)))

(defn annotate-jointly
  "Returns the Wikipedia articles relevant for the given strings by concatenating the strings and running wikification on the result.
   The intersecting annotations are resolved, and for each acticle the occurrence with the strongest weight is selected. "
  [strings prob]
  (println "joint annotation")
  (let [->wapi-article-cached (comp (memoize mk-wapi-article) get-id)
        joint-string (string/join ". " strings)
        doc-topics (map vector strings (get-wiki-topics-jointly strings prob))
        article-link (fn [[^Topic topic ^Position position]]
                       (let [wapi-article (->wapi-article-cached topic)
                             start (.getStart position)
                             end (.getEnd position)]
                         (when wapi-article
                           (->
                            (wapi/->ArticleLink #_ string wapi-article
                                                (.substring joint-string start end)
                                                (.getWeight topic))
                            (assoc :start start :end end)))))]
    (for [[string topic-pos] doc-topics]
      (->> topic-pos
           (keep article-link)
           remove-single-word-links
           resolve-collisions-as-wminer
           wapi/select-max-strength))))

(defn annotate-chunked
  "Returns the Wikipedia articles relevant for the given strings by dividing the collection of strings into chunks of a given size and
   running annotate-jointly on each chunk."
  [snippets chunk-size & [prob]]
  {:pre [(<= 1 chunk-size)]}
  (println "chunked annotation")
  (let [chunks (partition-all chunk-size snippets)]
    (doall (mapcat #(annotate-jointly % prob) chunks))))

(deftype WikiService []
  wapi/IWikiService
  (-annotate [this strings]
    (wapi/-annotate this strings 5e-1))
  (-annotate [this strings prob]
    (annotate-chunked strings 100 prob))
  (-relatedness [this article-pairs]
    (for [[a1 a2] article-pairs
          :let [wminer-a1 (->wminer-article a1)
                wminer-a2 (->wminer-article a2)
                score (.getRelatedness (art-comparer) wminer-a1 wminer-a2)]]
      score))
  (-article-categories [this article]
    (map ->wapi-category (.getParentCategories (->wminer-article article))))
  (-cat-relations [this categories]
    (let [wiki (wikipedia)
          categories (set categories)]
      (for [cat categories
            parent (filter categories (parent-cats cat))]
        [cat parent])))
  (-search [this query opt]
    (letfn [(mk-article [^Label$Sense sense]
              (when-let [article (->wapi-article sense)]
                (assoc article :prob (get-prob sense))))]
      (->> (get-senses* query opt)
           (keep mk-article)))))

(def service (WikiService.))
