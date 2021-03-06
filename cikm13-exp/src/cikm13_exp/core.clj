(ns cikm13-exp.core
  (:require [clojure [string :as string]
                     [set :as set]]
            [clojure.java [io :as io]]
            [utils.core :as u]
            [mas-api.core :as mas]
            [topic-maps.core :as tmaps]
            [topic-maps.features :as ftr]
            [wiki-api.core :as wapi]
            [graphs.core :as g]
            [seq-learn.core :as sl]
            [seq-learn.dagger :as da]
            [scaiella12topical [core :as sca]])
  (:import [topic_maps.core Topic]))

(defrecord Paper [title abstract]
  wapi/IDocument
    (doc-string [this] (str (:title this) ". " (:abstract this))))

(extend-type Paper
  tmaps/IDocument
    (doc-id [this] (str (:title this)" " (:year this) " " (doall (string/join ", " (map :last-name (:author this))))))
    (doc-string [this] (str (:title this) ". " (:abstract this))))

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

(defrecord TopicSubmap [topic-map topics submap prev-state features feature-vals]
  sl/IState
  (previous-state [this]
    prev-state)
  (state-name [this]
    (str (:title (:main-topic topic-map)) " : " (string/join ", " (map :title topics))))
  (next-actions [this]
    (remove (set topics) (tmaps/get-topics topic-map)))
  (next-state [this topic]
    (let [new-topics (conj topics topic)]
          (TopicSubmap. topic-map
                        new-topics 
                        (tmaps/submap topic-map new-topics :keep [:main-topic])
                        this
                        features
                        (sl/compute-features this topic))))
  sl/IActionFeatures
    (compute-features [this] feature-vals)
    (compute-features [this new-topic]
      (mapv #(%1 topic-map submap %2 new-topic) features feature-vals)))

(extend-type Topic sl/IAction
  (action-name [this] (:title this)))

(extend-type wiki_api.core.Article
  sl/IAction
  (action-name [this] (wapi/article-title this)))

(defn empty-submap [topic-map features]
  (let [submap (tmaps/submap topic-map [] :keep [:main-topic])]
    (TopicSubmap. topic-map []
                  submap 
                  nil
                  features
                  (mapv #(%1 topic-map) features))))

(defrecord StateLoss [loss-fn]
  da/IStateLoss
  (compute-state-loss [this state1 state2]
    (loss-fn state1 state2)))

(defrecord ActionLoss [loss-fn]
  da/IActionLoss
  (compute-action-loss [this state action1 action2]
    (loss-fn state action1 action2)))

(defrecord StateActionLoss [loss-fn]
  da/IStateActionLoss
  (compute-state-action-loss [this optimal-state given-state action]
    (loss-fn optimal-state given-state action)))

(defn zero-one [a b] (if (= a b) 0 1))

(def action-loss-01 (ActionLoss. (fn [state action1 action2] (zero-one action1 action2))))

(def state-loss-01 (StateLoss. (fn [state1 state2] (zero-one state1 state2))))

(defn match-score [topic-map optimal-topics predicted-topics]
  (let [topic-graph (:topic-graph topic-map)
        ; start костыль
        title-topic-map (u/key-map :title (tmaps/get-topics topic-map))
        predicted-topics (map (comp title-topic-map :title) predicted-topics)
        optimal-topics (map (comp title-topic-map :title) optimal-topics)
        ; end костыль
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
        _ (println "Optimal: " optimal-topics)
        _ (println "Predicted: " predicted-topics)
        iter (fn [scores optimal predicted]
               (if (empty? optimal)
                 scores
                 (let [topic (first optimal)
                       match (best-match topic predicted)
                       score (topic-similarity topic match)]
                   (recur (conj scores score) (rest optimal) (disj predicted match)))))
        scores (iter '() optimal-topics predicted-topics)]
    (/ (u/avg scores) (+ 1 equality-bonus))))

(defn match-score* [topic-map optimal-topics predicted-map]
  (let [topic-graph (:topic-graph topic-map)
        ; start костыль
        title-topic-map (u/key-map :title (tmaps/get-topics topic-map))
        predicted-topics (tmaps/get-topics predicted-map)
        optimal-topics (map (comp title-topic-map :title) optimal-topics)
        ; end костыль
        seq-length (count optimal-topics)
        predicted-topics (set predicted-topics)
        _ (when (not= (count predicted-topics) (count optimal-topics))
            (println "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! topic sequences of unequal sizes")
            (println optimal-topics)
            (println predicted-topics)
            (println "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"))
        optimal-topic-docs (u/val-map #(set (ftr/covered-docs topic-map [%])) (set optimal-topics ))
        predicted-topic-docs #(set (tmaps/proper-docs predicted-map %))
        set-overlap #(/ (count (set/intersection %1 %2))
                        (count (set/union %1 %2)))
        equality-bonus 0.1
        topic-similarity #(+ (set-overlap (optimal-topic-docs %1) (predicted-topic-docs %2))
                             (if (= (:title %1) (:title %2)) equality-bonus 0))
        best-match (fn [topic topics]
                     (apply max-key (partial topic-similarity topic) topics))
;       _ (println "Optimal: " optimal-topics)
;       _ (println "Predicted: " predicted-topics)
        iter (fn [scores optimal predicted]
               (if (empty? optimal)
                 scores
                 (let [topic (first optimal)
                       match (best-match topic predicted)
                       score (topic-similarity topic match)]
                   (recur (conj scores score) (rest optimal) (disj predicted match)))))
        scores (iter '() optimal-topics predicted-topics)]
    (/ (u/avg scores) (+ 1 equality-bonus))))

;(def smart-matching-loss 
;  (StateLoss. (fn smart-matching-loss [optimal predicted]
;           (let [topic-map (:topic-map optimal)
;                 optimal-topics (:topics optimal)
;                 predicted-topics (:topics predicted)]
;             (- 1 (match-score topic-map optimal-topics predicted-topics))))))

(def smart-matching-loss 
  (StateActionLoss. (fn smart-matching-loss [optimal-state given-state action]
           (let [topic-map (:topic-map optimal-state)
                 optimal-topics (:topics optimal-state)
                 predicted-topics (conj (:topics given-state) action)]
             (- 1 (match-score topic-map optimal-topics predicted-topics))))))

(def features
 ; (Features. (vec (concat
                    [
#_                   ftr/n-links 
                      ftr/avg-pairwise-dist ; 1
                      ftr/n-connected ; 2
                      ftr/paper-coverage ; 3
                      ftr/direct-doc-coverage ; 4
#_                    ftr/unevenness ; 
                      ftr/avg-topic-freq ; 5
                      ftr/min-topic-freq ; 6
                      ftr/avg-cum-freq ; 7
                      ftr/min-cum-freq ; 8 
#_                    ftr/height ;  
                      ftr/partition-coef ; 9 
                      ftr/main-subtopics ; 10 
#_                    ftr/avg-pc-overlap ;   
#_                    ftr/max-pc-overlap ;   
                      ftr/avg-overlap ; 11 
                      ftr/max-overlap ; 12 
#_                    #(ftr/subtopic-coverage %1 %2 :direct true) ;
#_                    #(ftr/avg-n-adj %1 %2 :mode :child) ; 
#_                    #(ftr/avg-n-adj %1 %2 :mode :parent) ; 
#_                    #(ftr/max-n-adj %1 %2 :mode :parent)] ;
                    );))) 

(defn read-ground-truth [query features]
  (let [topic-map (->> (input-file-name query) slurp read-string tmaps/cache-merged tmaps/cache-topic-dist)
        {:keys [topic-graph]} topic-map
        topic-names (read-string (slurp (output-file-name query)))
        topic-name-map (u/key-map :title (g/get-nodes topic-graph))
        topic-by-name #(or (topic-name-map %) (throw (Exception. (str "No topic for name: " %))))
        topics (mapv topic-by-name topic-names)]
    [(empty-submap topic-map features) topics]))

(defn state-action-seqs [queries features]
  (->> queries (map #(read-ground-truth % features))))

(defn accuracy-at [true-seq predicted-seq]
  (/ (count (filter (set predicted-seq) true-seq))
     (count true-seq)
     1.0))

(defn accuracies-at [optimal-seq predicted-seq]
 (mapv #(accuracy-at (take % optimal-seq) (take % predicted-seq)) (range 1 (inc (count optimal-seq)))))

(defn match-score-at [topic-map optimal-seq predicted-seq]
 (mapv #(match-score topic-map (take % optimal-seq) (take % predicted-seq)) (range 1 (inc (count optimal-seq)))))

(defn accuracies [model query features]
  (let [[init-state optimal-seq] (read-ground-truth query features)
        n (count optimal-seq)
        topic-map (:topic-map init-state)
        predicted-seq (sl/best-action-seq model init-state n)]
    (println "Optimal sequence:" (map :title optimal-seq))
    (println "Predicted sequence:" (map :title predicted-seq))
    (println "Accuracies: " (accuracies-at optimal-seq predicted-seq))
    (println "Match-score: " (match-score-at topic-map optimal-seq predicted-seq))
    (tmaps/display-topics (tmaps/submap topic-map optimal-seq))
    (tmaps/display-topics (tmaps/submap topic-map predicted-seq))))

;(defn train-model [queries & {:as opt}]
;  (let [input-data (state-action-seqs queries)]
;    (sl/train-model input-data topic-overlap-loss features :c (:c opt))))

(defn train-dagger [queries features seq-length n-iter & {:keys [evaluate]}]
  (let [input-data (state-action-seqs queries features)]
    (da/dagger input-data seq-length
               #_ topic-overlap-loss
               smart-matching-loss
               action-loss-01
               n-iter
               :evaluate evaluate)))

(defn run-fold [queries features ntopics niter fold]
  (let [query (get queries fold)]
    (train-dagger (remove #{query} queries) features ntopics niter :evaluate #(accuracies % query features))))

(defn leave-one-out [queries features ntopics niter]
  (doseq [query queries]
    (train-dagger (remove #{query} queries) features ntopics niter :evaluate #(accuracies % query features))))

(defn predict-compare [model query features n]
  (let [[init-state optimal-seq] (read-ground-truth query features)
        topic-map (:topic-map init-state)
        predicted-seq (sl/best-action-seq model init-state n)]
    (println "Optimal sequence:" (map :title optimal-seq))
    (println "Predicted sequence:" (map :title predicted-seq))
 #_ (tmaps/display-topics topic-map)
 #_ (tmaps/display-topics (tmaps/submap topic-map optimal-seq))
 #_ (tmaps/display-topics (tmaps/submap topic-map predicted-seq))))

(defn assess-baseline [query features]
  (let [[init-state optimal-seq] (read-ground-truth query features)
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

(defrecord ScaResult [query optimal topics nclusters nmerge accuracy match-score])

(defrecord BestResults [best-accuracy best-match])

(defn assess-sca [queries features nclusters]
  (let [optimal-seq (memoize (fn [query] (second (read-ground-truth query features))))
        full-topic-map (memoize (fn [query] (:topic-map (first (read-ground-truth query features)))))
        sca-params (memoize
                     (fn [query]
                       (let [search (mas/search-papers query :end 100 :timeout 50000)
                             _ (when (u/fail? search) (throw (Exception. (str (:error search)))))
                             papers (map map->Paper (:value search))]
                         (sca/prepare-params papers))))
        single-result (fn [query nmerge k]
                        (let [params (assoc (sca-params query)
                                            :nmerge nmerge
                                            :nclusters k)
                              predicted-topic-map (sca/build-topic-map* params)
                              predicted-topics (tmaps/get-topics predicted-topic-map)
                              optimal-topics (take k (optimal-seq query))
                              accuracy (accuracy-at (map :title optimal-topics) (map :title predicted-topics))
                              ;match-score (match-score* (full-topic-map query) optimal-topics predicted-topic-map)
                              match-score (match-score (full-topic-map query) optimal-topics predicted-topics)]
                          (doto (ScaResult. query optimal-topics predicted-topics k nmerge accuracy match-score) println)))
        avg-results (fn [queries nmerge k]
                      (let [
                          ; best-nmerge-acc {1 5 2 5 3 5 4 5 5 5 6 5 7 4 8 4}
                           best-nmerge-match {1 5 2 4 3 5 4 4 5 3 6 4 7 4 8 4}
                       ; new   [5 4 5 4 3 4 4 4]
                       ;    results (doall (for [query queries]
                       ;              (single-result query (best-nmerge-acc k) k)))
                       ;    ; [5 5 5 5 5 5 4 4]
                       ;    ; [5 5 5 5 5 4 4 2]
                       ;    _ (println "Accuracies at" k (mapv :accuracy results))
                            results (doall (for [query queries]
                                      (single-result query nmerge #_(best-nmerge-match k) k)))
                            _ (println "Match scores at" k (mapv :match-score results))
                            avg-accuracy nil; (u/avg (mapv :accuracy results))
                            avg-match-score (u/avg (mapv :match-score results))]
                        (ScaResult. nil nil nil k nmerge avg-accuracy avg-match-score)))
        best-results (doall (for [k (rest (range (inc nclusters)))]
                       (let [results (for [nmerge (range 1 6)]
                                       (avg-results queries nmerge k))
                             best-acc nil; (apply max-key :accuracy results)
                             best-match (apply max-key :match-score results)]
                         (BestResults. best-acc best-match))))]
;  (println "Best by accuracy:") 
;  (println "Accuracies: " (mapv (comp :accuracy :best-accuracy) best-results))
;  (println "Match-score: " (mapv (comp :match-score :best-accuracy) best-results))
;  (println "Params: " (mapv (comp :nmerge :best-accuracy) best-results))
   (println "Best by match-score") 
   (println "Accuracies: " (mapv (comp :accuracy :best-match) best-results))
   (println "Match-score: " (mapv (comp :match-score :best-match) best-results))
   (println "Params: " (mapv (comp :nmerge :best-match) best-results))
))

(defn build-sca-map [query & more]
  (let [search (mas/search-papers query :end 100 :timeout 30000)
        _ (when (u/fail? search) (throw (Exception. (str (:error search)))))
        papers (map map->Paper (:value search))
        topic-map (apply sca/build-topic-map papers more)]
    (tmaps/display-topics topic-map)))

(defn display-topics [query topic-names]
  (let [[init-state optimal-seq] (read-ground-truth query features)
        topic-map (:topic-map init-state)
        topic-names (set topic-names)
        all-topics (tmaps/get-topics topic-map)
        topics (filter (comp topic-names :title) all-topics)
        submap (tmaps/submap topic-map topics)]
    (tmaps/display-topics submap)))

(defn -main [#_ arg & more]
  (assess-sca queries features 8)
  #_(let [fold (Integer/parseInt arg)]
    (run-fold queries features 8 10 fold))
  #_(leave-one-out queries features 8 10)
  #_(build-sca-map "graph algorithms")
  #_(build-sca-map "statistical relational learning" :nmerge 0)
  #_(build-sca-map "neural networks"))
