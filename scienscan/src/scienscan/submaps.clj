(ns scienscan.submaps
  (:require
   [clojure.string :as string]
   [compojure.handler :as handler]
   [compojure.route :as route]
   [cheshire [core :as json]]
   [utils.core :as u]
   [topic-maps.core :as tmaps]
   [seq-learn.core :as sl]
   [learn-submap.lrn-sbm :as lrn]))

(defn build-topic-map
  "Build the topic map from the search results (documents)."
  [s-results]
  (let [time (System/currentTimeMillis)]
    (-> s-results
        (doto (#(println (count %) "results")))
        tmaps/init-topic-map
        tmaps/link-to-articles
        (doto (#(println (count (tmaps/get-topics %)) "articles in" (/ (- (System/currentTimeMillis) time) 1000.0))))
        ;; tmaps/remove-singleton-articles
        ;; (doto (#(println (count (tmaps/get-topics %)) "articles after removing singletons")))
        tmaps/retrieve-categories
        (doto (#(println (count (tmaps/get-topics %)) "articles and categories")))
        tmaps/link-categories
        (doto ((fn [_] (println "categories linked"))))
        tmaps/merge-similar
        (doto (#(println (count (tmaps/get-topics %)) "articles and categories after merge")))
        tmaps/break-loops
        (doto ((fn [_] (println "cycles broken"))))
        (u/assocf main-topic identity :main-topic)
        (doto (#(println "Main topic:" (:main-topic %))))
        expand-main-topic
        (#(submap % (get-topics %)))
        (doto ((fn [_] (println "Computed the submap")))))))

(defprotocol ISummarizer
  "Abstraction for topic summarization algorithms."
  (build-summary [this topic-map n-topics]
    "Builds the topic summary and returns the new summarizer.
     The built summary should be obtained from the new summarizer using get-summary.
     This two-step procedure allows the summarizer to chache intermediate results
     and reuse them when building new summaries (if necessary for performance):

     (let [summarizer1 (build-summary summarizer0 a-topic-map 10)
           smaller-summary (get-summary summarizer1)
           _ (dostuff! smaller-summary)
           summarizer2  (build-summary summarizer1 a-topic-map 11) ; potentially more efficient
           bigger-summary (get-summary summarizer2)
           _ (dustuff! bigger-summary)])")
  (get-summary [this]
    "Obtains the last-built summary from the summarizer."))

;; Summarizer that builds the topic summary from scratch using given summarization function, without caching any intermediate results.
;; Consists of:
;; - summarize-fn : summarization function
;; - result : stores the built summary
;; Use (simple-summarizer summarize-fn) instead of (SimpleSummarizer. ...)
(deftype SimpleSummarizer [summarize-fn result]
  ISummarizer
  (build-summary [this topic-map n-topics]
    (assoc this :result (summarize-fn topic-map n-topics)))
  (get-summary [this]
    result))

(defn simple-summarizer
  "Builds the instance of SimpleSummarizer."
  [summarize-fn]
  (SimpleSummarizer. summarize-fn nil))

(defn best-by-greedy-coverage
  "Topic summarization function that greedily optimizes the (non-transitive) document coverage."
  [topic-map n-topics]
  (let [all-topics (tmaps/get-topics topic-map)
        iter  (fn [topics covered-docs]
                (if (= n-topics (count topics))
                  topics
                  (let [candidate-topics (remove (set topics) all-topics)
                        docs-covered-with (u/val-map
                                           #(into covered-docs
                                                  (tmaps/proper-docs topic-map %))
                                           candidate-topics)
                        score-fn (comp count docs-covered-with)
                        next-topic (apply max-key score-fn candidate-topics)]
                    (recur (conj topics next-topic)
                           (docs-covered-with next-topic)))))
        topics (iter [] #{})]
    (tmaps/submap topic-map topics)))

(def mock-summarizer
  "Mock summarizer that selects the bottom n-topics from the list ordered by the topic title."
  (simple-summarizer
    (fn summarize-fn [topic-map n-topics]
      (->> (tmaps/get-topics topic-map)
           (sort-by :title)
           (take n-topics)
           (tmaps/submap topic-map)))))

(def greedy-coverage
  "Summarizer that selects topcis by greedily optimizing the document coverage."
  (simple-summarizer best-by-greedy-coverage))


;; (defn- grow-summaries
;;   "Takes a vector of topic summaries (lrn/TopicSubmap) of sizes from 0 to k, and returns
;;    a bigger vector of topic summaries of sizes from 0 to n-topics > k.
;;    Each next summary is obtained form the previous by applying step-fn."
;;   [summaries topic-map step-fn n-topics]
;;   {:pre [(<= (count summaries) n-topics)]}
;;   (letfn [iter (fn iter [result]
;;                  (cond (empty? result)))]))


;; sl/IModel lrn/TopicSubmap Integer -> lrn/TopicSubmap
(defn- grow-submap
  "Grows the topic submap to the size n-topics using the model."
  [model topic-submap n-topics]
  {:pre [(< (count (:topics topic-submap)) n-topics)]}
  (let [
        ;; iter (fn iter [state]
    ;;            (if (= (count (:topics state)) n-topics)
    ;;              state
    ;;              (recur (sl/next-state state (sl/best-action model state)))))]
    ;; (iter topic-submap)
        ]
    (println "Growing the topic submap from size" (count (:topics topic-submap)) "to size" n-topics)
    (->> topic-submap
         (iterate #(sl/next-state % (sl/best-action model %)))
         (drop-while #(< (count (:topics %)) n-topics))
         first)))

;; lrn/TopicSubmap Integer -> lrn/TopicSubmap
(defn- shrink-submap [topic-submap n-topics]
  "Selects a smaller topic submap of size n-topics from a bigger submap,
   by iterating through the previous states of the submap."
  [topic-submap n-topics]
  {:pre [(< (count (:topics topic-submap)) n-topics)]}
  (let [
;; iter (fn iter [state]
;;                (if (= (count (:topics state)) n-topics)
;;                  state
;;                  (recur (sl/previous-state state)))))]
;;    (iter topic-submap)                 
]
    (println "Shrinking the topic submap from size" (count (:topics topic-submap)) "to size" n-topics)
    (->> topic-submap
         (iterate sl/previous-state)
         (drop-while #(> (count (:topics %)) n-topics))
         first)))

;; Summarizer that builds the topic summaries sequentially using DAgger
;; features : vector of functions
;; model : sl/IModel
;; topic-submap : lrn/TopicSubmap
;; result : tmaps/TopicMap
(deftype DaggerCachingSummarizer [features model topic-submap result]
  ISummarizer
  (build-summary [this topic-map n-topics]
    (println "summarizing into" n-topics "topics")
    ;; {:pre [(instance? topic_maps.core.TopicMap topic-map)
    ;;        (>= (count (tmaps/get-topics topic-map)) n-topics)]
    ;;  :post [(not (nil? %))
    ;;         (instance? topic_maps.core.TopicMap %)
    ;;         (= (count (tmaps/get-topics %)) n-topics)]}
    (if (or (nil? topic-submap)
            (< (count (:topics topic-submap)) n-topics))
      (let [empty-submap (lrn/empty-submap topic-map features)
            topic-submap* (or topic-submap empty-submap)
            topic-submap* (grow-submap model topic-submap* n-topics)]
        (DaggerCachingSummarizer. features  model topic-submap* (:submap topic-submap*)))
      (let [small-submap (shrink-submap topic-submap n-topics)]
        (DaggerCachingSummarizer. features model topic-submap (:submap small-submap)))))
  (get-summary [this] result))

(defn dagger-caching-summarizer [features model]
  (DaggerCachingSummarizer. features model nil nil))

(def dagger-summarizer
  (dagger-caching-summarizer
   (-> (lrn/new-app) :conf :features)
   (sl/load-model "/Users/dmirylenka/code/asearch-modular/learn-submap/resources/models/aminer-wminer-10q-13-10-09.model")))
