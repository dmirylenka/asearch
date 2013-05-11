(ns wminer.core
  (:import [org.wikipedia.miner.util WikipediaConfiguration]
           [org.wikipedia.miner.model Wikipedia Article Category Page Page$PageType]
           [org.wikipedia.miner.annotation Disambiguator TopicDetector Topic]
           [org.wikipedia.miner.annotation.weighting LinkDetector])
  (:require (clojure.java [io :as io])
            (utils [core :as u]
                   [text :as t])
            (clojure [string :as string])))

(def resources
  (delay 
    (let [wiki-conf (WikipediaConfiguration. (io/file "/Users/dmirylenka/code/asearch-modular/wminer/resources/wikipedia-template.xml")) 
          wikipedia (Wikipedia. wiki-conf false)
          disambiguator (Disambiguator. wikipedia)
          topic-detector (TopicDetector. wikipedia disambiguator true false)
          link-detector (LinkDetector. wikipedia)]
      {:wikipedia wikipedia
       :topic-detector topic-detector
       :link-detector link-detector})))

(defn ^TopicDetector topic-detector []
  (@resources :topic-detector))

(defn ^LinkDetector link-detector []
  (@resources :link-detector))

(defn get-wiki-topics
  "Returns a collection of wikiminer Topics detected in a string."
  [^String string & [probability]]
  (let [all-topics (.getTopics (topic-detector) string nil)]
    (if-not probability
      all-topics
      (.getBestTopics (link-detector) all-topics probability))))

(defn- get-title [^Topic topic]
  (.getTitle topic))

(defn get-topics
  "Same as get-wiki-topics, but returns only topic titles in the format as in the URLs of Wikipedia articles."
  [string & [probability]]
  (map get-title (get-wiki-topics string probability)))

(defn article-by-title [title]
  (.getArticleByTitle (@resources :wikipedia) title))

(defn out-links [title]
  (map (memfn getTitle)
       (.getLinksOut (article-by-title title))))

(defn cat-by-title [title]
  (.getCategoryByTitle (@resources :wikipedia) title))

(defn page-by-id [title]
  (.getPageById (@resources :wikipedia) title))

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
