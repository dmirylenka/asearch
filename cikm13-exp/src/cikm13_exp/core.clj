(ns cikm13-exp.core
  (:require [clojure [string :as string]
                     [set :as set]]
            [clojure.java [io :as io]]
            [utils.core :as u]
            [mas-api.core :as mas]
            [topic-maps.core :as tmaps]
            [topic-maps.features :as ftr]
            [graphs.core :as g]
            [seq-learn.core :as sl]
            [seq-learn.dagger :as da])
  (:import [topic_maps.core Topic]))

(defrecord Paper [title abstract]
  tmaps/IDocument
    (doc-id [this] (str title " " (:year this) " " (doall (string/join ", " (map :last-name (:author this))))))
    (doc-string [this] (str title ". " abstract)))

(def input-data-dir "./resources/data/")

(defn input-file-name [query] (str input-data-dir query ".in.clj"))

(defn output-file-name [query] (str input-data-dir query ".out.clj"))

(defn fetch-store-input [query]
  (let [search (mas/search-papers query :end 100 :timeout 30000)
        _ (when (u/fail? search) (throw (Exception. (:error search))))
        papers (map map->Paper (:value search))
        topic-map (tmaps/prepare-map papers)]
    (spit (input-file-name query) (pr-str topic-map))))

(defn display-input [query]
  (let [topic-map (read-string (slurp (input-file-name query)))]
    (tmaps/display-topics topic-map)))

(defn fetch [query]
  (fetch-store-input query)
  (display-input query))

(def queries ["dimensionality reduction" "fourier series" "anomaly detection" "statistical relational learning" "clustering methods" "latent dirichlet allocation" "neural networks" "quadratic programming" "numerical differential equations" "graph algorithms"])

(declare ->TopicSubmap)

(extend-type Topic sl/IAction
  (action-name [this] (:title this))
  (-next-state [this state]
    (let [{:keys [topic-map topics]} state]
      (->TopicSubmap topic-map (conj topics this)))))

(defrecord TopicSubmap [topic-map topics]
  sl/IState
  (previous-state [this]
    (if (empty? topics)
      nil
      (->TopicSubmap topic-map (pop topics))))
  (state-name [this]
    (str (:title (:main-topic topic-map)) " : " (string/join ", " (map :title topics))))
  (next-actions [this]
    (remove (set topics) (g/get-nodes (:topic-graph topic-map)))))

(defn empty-submap [topic-map]
  (->TopicSubmap topic-map []))

(defrecord Loss [loss-fn]
  sl/ILoss
  (compute-loss [this state1 state2]
    (loss-fn state1 state2)))

(def loss01 (Loss. #(if (= %1 %2) 0 1)))

(def topic-overlap-loss
  (Loss. #(let [topics1 (set (:topics %1)) topics2 (set (:topics %2))]
            (- 1 (/ (count (set/intersection topics1 topics2))
                    (count (set/union topics1 topics2)))))))

(defn match-score [topic-map optimal-topics predicted-topics]
  (let [topic-graph (:topic-graph topic-map)
        seq-length (count optimal-topics)
        predicted-topics (set predicted-topics)
        topic-docs (u/val-map #(set (ftr/covered-docs topic-map [%]))
                              (set (concat optimal-topics predicted-topics)))
        set-overlap #(/ (count (set/intersection %1 %2))
                        (count (set/union %1 %2)))
        equality-bonus 0.1
        topic-similarity #(+ (set-overlap (topic-docs %1) (topic-docs %2))
                             (if (= %1 %2) equality-bonus 0))
        best-match (fn [topic topics]
                     (apply max-key (partial topic-similarity topic) topics))
        iter (fn [scores optimal predicted]
               (if (empty? optimal)
                 scores
                 (let [topic (first optimal)
                       match (best-match topic predicted)
                       score (topic-similarity topic match)]
                   (recur (conj scores score) (rest optimal) (disj predicted match)))))
        scores (iter '() optimal-topics predicted-topics)]
    (/ (u/avg scores) (+ 1 equality-bonus))))

(def smart-matching-loss 
  (Loss. (fn smart-matching-loss [optimal predicted]
           (let [topic-map (:topic-map optimal)
                 optimal-topics (:topics optimal)
                 predicted-topics (:topics predicted)]
             (- 1 (match-score topic-map optimal-topics predicted-topics))))))

(defrecord Features [feature-fns]
  sl/IFeatures
  (-features [this state]
    (let [topic-map (:topic-map state)
          submap (tmaps/submap topic-map (:topics state) :keep [:main-topic])]
    (mapv #(% topic-map submap) feature-fns))))

(def features
  (Features. (vec (concat
                    [ftr/n-links ; 1
                     ftr/avg-pairwise-dist ; 2
                     ftr/n-connected ; 3
                     ftr/paper-coverage ; 4
                     ftr/direct-doc-coverage ; 5
                     ftr/unevenness ; 6
                     ftr/avg-topic-freq ; 7
                     ftr/min-topic-freq ; 8
                     ftr/avg-cum-freq ; 9
                     ftr/min-cum-freq ; 10 
                     ftr/height ; 11
                     ftr/partition-coef ; 12
                     ftr/main-subtopics ; 13
                     ftr/avg-pc-overlap ; 14
                     ftr/max-pc-overlap ; 15
                     ftr/avg-overlap ; 16
                     ftr/max-overlap ; 17
                     #(ftr/subtopic-coverage %1 %2 :direct true) ; 18
                     #(ftr/avg-n-adj %1 %2 :mode :child) ; 19
                     #(ftr/avg-n-adj %1 %2 :mode :parent) ; 20 
                     #(ftr/max-n-adj %1 %2 :mode :parent)] ; 21
                    )))) ; 25-28

(defn read-ground-truth [query]
  (let [topic-map (tmaps/cache-merged (read-string (slurp (input-file-name query))))
        {:keys [topic-graph]} topic-map
        topic-names (read-string (slurp (output-file-name query)))
        topic-name-map (u/key-map :title (g/get-nodes topic-graph))
        topic-by-name #(or (topic-name-map %) (throw (Exception. (str "No topic for name: " %))))
        topics (mapv topic-by-name topic-names)]
    [(empty-submap topic-map) topics]))

(defn state-action-seqs [queries]
  (->> queries (map read-ground-truth)))

(defn accuracy-at [true-seq predicted-seq]
  (/ (count (filter (set predicted-seq) true-seq))
     (count true-seq)
     1.0))

(defn accuracies-at [optimal-seq predicted-seq]
 (mapv #(accuracy-at (take % optimal-seq) (take % predicted-seq)) (range 1 (inc (count optimal-seq)))))

(defn match-score-at [topic-map optimal-seq predicted-seq]
 (mapv #(match-score topic-map (take % optimal-seq) (take % predicted-seq)) (range 1 (inc (count optimal-seq)))))

(defn accuracies [model query]
  (let [[init-state optimal-seq] (read-ground-truth query)
        n (count optimal-seq)
        topic-map (:topic-map init-state)
        predicted-seq (sl/best-action-seq model init-state n)]
    (println "Optimal sequence:" (map :title optimal-seq))
    (println "Predicted sequence:" (map :title predicted-seq))
    (println "Accuracies: " (accuracies-at optimal-seq predicted-seq))
    (println "Match-score: " (match-score-at topic-map optimal-seq predicted-seq))
    (tmaps/display-topics (tmaps/submap topic-map optimal-seq))
    (tmaps/display-topics (tmaps/submap topic-map predicted-seq))))

(defn train-model [queries & {:as opt}]
  (let [input-data (state-action-seqs queries)]
    (sl/train-model input-data topic-overlap-loss features :c (:c opt))))

(defn train-dagger [queries seq-length n-iter & {:keys [evaluate]}]
  (let [input-data (state-action-seqs queries)]
    (da/dagger input-data seq-length
               #_ topic-overlap-loss
                smart-matching-loss
               features n-iter :evaluate evaluate)))

(defn leave-one-out [queries]
  (doseq [query queries]
    (train-dagger (remove #{query} queries) 8 10 :evaluate #(accuracies % query))))

(defn predict-compare [model query n]
  (let [[init-state optimal-seq] (read-ground-truth query)
        topic-map (:topic-map init-state)
        predicted-seq (sl/best-action-seq model init-state n)]
    (println "Optimal sequence:" (map :title optimal-seq))
    (println "Predicted sequence:" (map :title predicted-seq))
 #_ (tmaps/display-topics topic-map)
 #_ (tmaps/display-topics (tmaps/submap topic-map optimal-seq))
 #_ (tmaps/display-topics (tmaps/submap topic-map predicted-seq))))

(defn assess-baseline [query]
  (let [[init-state optimal-seq] (read-ground-truth query)
        topic-map (:topic-map init-state)
        n (count optimal-seq)
        all-topics (tmaps/get-topics topic-map)
        iter (fn [predicted-seq covered-docs]
               (if (= n (count predicted-seq))
                 predicted-seq
                 (let [;covered-docs (set (mapcat #(proper-docs topic-map %)))
                       candidate-topics (remove (set predicted-seq) all-topics)
                       docs-covered-with (u/val-map #(into covered-docs (tmaps/proper-docs topic-map %)) candidate-topics)
                       score-fn (comp count docs-covered-with)
                       next-topic (apply max-key score-fn candidate-topics)]
                   (recur (conj predicted-seq next-topic) (docs-covered-with next-topic)))))
        predicted-seq (iter [] #{})]
    (println "Optimal sequence:" (map :title optimal-seq))
    (println "Predicted sequence:" (map :title predicted-seq))
    (println "Accuracies: " (accuracies-at optimal-seq predicted-seq))
    (println "Match-score: " (match-score-at topic-map optimal-seq predicted-seq))
    (tmaps/display-topics (tmaps/submap topic-map optimal-seq))
    (tmaps/display-topics (tmaps/submap topic-map predicted-seq))))

(defn -main [])
