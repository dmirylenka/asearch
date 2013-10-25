(ns scienscan.submaps
  (:require
   [clojure.string :as string]
   [compojure.handler :as handler]
   [compojure.route :as route]
   [cheshire [core :as json]]
   [utils.core :as u]
   [topic-maps.core :as tmaps]
   [seq-learn.core :as sl]
   [learn-submap.lrn-sbm :as lrn]
   [learn-submap.database :as lrndb]))

(defn test-nil-topics [topic-map]
  (let [topics (tmaps/get-topics topic-map)]
    (when (some nil? topics)
      (println "Nil topic detected!!"))
    topic-map))

(defn build-topic-map
  "Build the topic map from the search results (documents)."
  [s-results]
  (let [time (System/currentTimeMillis)]
    (-> s-results
        (doto (#(println (count %) "results")))
        tmaps/init-topic-map
        (test-nil-topics)
        tmaps/link-to-articles
        (test-nil-topics)
        (doto (#(println (count (tmaps/get-topics %)) "articles in" (/ (- (System/currentTimeMillis) time) 1000.0))))
        ;; tmaps/remove-singleton-articles
        ;; (doto (#(println (count (tmaps/get-topics %)) "articles after removing singletons")))
        tmaps/retrieve-categories
        (test-nil-topics)
        (doto (#(println (count (tmaps/get-topics %)) "articles and categories")))
        tmaps/link-categories
        (test-nil-topics)
        (doto ((fn [_] (println "categories linked"))))
        tmaps/merge-similar
        (test-nil-topics)
        (doto (#(println (count (tmaps/get-topics %)) "articles and categories after merge")))
        tmaps/break-loops
        (test-nil-topics)
        (doto ((fn [_] (println "cycles broken"))))
        (u/assocf tmaps/main-topic identity :main-topic)
        (test-nil-topics)
        (doto (#(println "Main topic:" (:main-topic %))))
        tmaps/expand-main-topic
        (test-nil-topics)
        (#(tmaps/submap % (tmaps/get-topics %)))
        (test-nil-topics)
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

(defprotocol IActiveSummarizer
  "An abstraction for a topic summarizer that queries the user for suggestion."
  (get-candidates [this]
    "Returns k candidates for the best topic to be added to the current summary.")
  (add-next-topic [this topic]))

;; Summarizer That builds the topic summary from scratch using given summarization function, without caching any intermediate results.
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


;; sl/IModel lrn/TopicSubmap Integer -> lrn/TopicSubmap
(defn- grow-submap
  "Grows the topic submap to the size n-topics using the model."
  [model topic-submap n-topics]
  {:pre [(or (zero? n-topics) (< (count (:topics topic-submap)) n-topics))]}
  (println "Growing the topic submap from size" (count (:topics topic-submap)) "to size" n-topics)
  (->> topic-submap
       (iterate #(sl/next-state % (sl/best-action model %)))
       (drop-while #(< (count (:topics %)) n-topics))
       first))

;; lrn/TopicSubmap Integer -> lrn/TopicSubmap
(defn- shrink-submap [topic-submap n-topics]
  "Selects a smaller topic submap of size n-topics from a bigger submap,
   by iterating through the previous states of the submap."
  [topic-submap n-topics]
  {:pre [(< (count (:topics topic-submap)) n-topics)]}
  (println "Shrinking the topic submap from size" (count (:topics topic-submap)) "to size" n-topics)
  (->> topic-submap
       (iterate sl/previous-state)
       (drop-while #(> (count (:topics %)) n-topics))
       first))

;; Summarizer that builds the topic summaries sequentially using DAgger
;; features : vector of functions
;; model : sl/IModel
;; topic-submap : lrn/TopicSubmap
;; result : tmaps/TopicMap
(defrecord DaggerCachingSummarizer [features model topic-submap result]
  ISummarizer
  (build-summary [this topic-map n-topics]
    (println "summarizing into" n-topics "topics")
    (if (or (nil? topic-submap)
            (< (count (:topics topic-submap)) n-topics))
      (let [empty-submap (lrn/empty-submap topic-map features)
            topic-submap* (or topic-submap empty-submap)
            topic-submap* (grow-submap model topic-submap* n-topics)]
        (DaggerCachingSummarizer. features  model topic-submap* (:submap topic-submap*)))
      (let [small-submap (shrink-submap topic-submap n-topics)]
        (DaggerCachingSummarizer. features model topic-submap (:submap small-submap)))))
  (get-summary [this] result)
  IActiveSummarizer
  (get-candidates [this]
    (sl/best-actions model topic-submap))
  (add-next-topic [this topic]
    (let [topic-submap* (sl/next-state topic-submap topic)]
      (DaggerCachingSummarizer. features model topic-submap* (:submap topic-submap*)))))

(defn save-ground-truth! [summarizer]
  (let [topic-submap (.topic-submap summarizer)
        init-state (lrn/get-init-state topic-submap)
        action-seq (:topics topic-submap)]
    (lrndb/save-ground-truth! (assoc init-state :features nil) action-seq)))

(defn retrain-summarizer [n-iter summarizer]
  (let [results (lrndb/read-ground-truth)
        topic-submap (.topic-submap summarizer)
        init-state (lrn/get-init-state topic-submap)
        action-seq (:topics topic-submap)
        input-data (cond-> results
                      init-state
                      (conj [init-state action-seq]))
        model (lrn/train-dagger* input-data n-iter)]
    (assoc summarizer :model model)))

(defn refresh-summarizer [summarizer]
  (assoc summarizer :topic-submap nil :result nil))

(defn get-k-candidates [smr k banned?]
  (->> smr get-candidates (remove banned?) (take k)))

(defn dagger-caching-summarizer [features model]
  (DaggerCachingSummarizer. features model nil nil))

(def dagger-summarizer
  (dagger-caching-summarizer
   (-> (lrn/new-app) :conf :features)
   (sl/load-model "/Users/dmirylenka/code/asearch-modular/learn-submap/resources/models/aminer-wminer-10q-13-10-09.model")))
