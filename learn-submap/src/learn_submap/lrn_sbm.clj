(ns learn-submap.lrn-sbm
  (:require [clojure [string :as string]
                     [set :as set]]
            [clojure.java [io :as io]]
            [utils.core :as u]
            [search-api.search-api :as sapi]
            [mas-api.core :as mas]
            [arxiv-api.core :as arxiv]
            [arnetminer.dataset :as aminer]
            [topic-maps.core :as tmaps]
            [topic-maps.features :as ftr]
            [wiki-api.core :as wapi]
            [graphs.core :as g]
            [seq-learn.core :as sl]
            [seq-learn.dagger :as da]))

(defn new-app [& {:keys [] :as opt}]
  {:conf (-> {:input-data-dir "./resources/data/"
              :features [
#_                   ftr/n-links 
;; NOTE: replaced average pairwise distance with the dumb feature
;;       because of the performance reasons
;; TODO: reimplement avg-pairwise-dist efficently
#_                      ftr/avg-pairwise-dist ; 1
#_                      ftr/always-one
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
#_                    #(ftr/max-n-adj %1 %2 :mode :parent)]}
             (merge opt))
   :data {
     :queries ["dimensionality reduction" "fourier series" "anomaly detection"
               "statistical relational learning" "clustering methods"
               "latent dirichlet allocation" "neural networks"
               "quadratic programming" "numerical differential equations"
               "graph algorithms"]}})

(defn input-file-name [conf query] (str (conf :input-data-dir) query ".in.clj"))

(defn output-file-name [conf query] (str (conf :input-data-dir) query ".out.clj"))

(defn mk-paper [{:keys [id author title abstract year] :as paper}]
  (let [doc-id (str title " " year " " (doall (string/join ", " (map #(str (:first-name %) " " (:last-name %)) author))))
        doc-string (str title ". " abstract)]
    (-> paper
        (assoc :doc-id doc-id
               :string doc-string))))

(defn fetch-store-input [conf query]
  (let [search-service (apply aminer/service (apply concat conf))
        search (sapi/search-papers search-service query :end 100 :timeout 30000)
        _ (when (u/fail? search) (throw (Exception. (pr-str (:error search)))))
        mk-doc (conf :mk-doc)
        _ (when (nil? mk-doc) (throw (Exception. "Nil :mk-doc parameter")))
        papers (map mk-doc (:value search))
        topic-map (tmaps/prepare-map papers)]
    (spit (input-file-name conf query) (pr-str topic-map))))

(defn display-input [conf query]
  (let [topic-map (read-string (slurp (input-file-name conf query)))]
    (tmaps/display-topics topic-map)))

(defn fetch [conf query]
  (fetch-store-input conf query)
  (display-input conf query))

;; An intermediate topic summary (submap) in the summary-growing process.
;; Consists of:
;; - topic-map : TopicMap being summarized
;; - topics : sequence of topics that constitutes the summary
;; - sumpap : TopicMap representing the actual summary. Stored for performance reasons.
;; - prev-state : the previous summary in the sequence, that is the sumarry from which the current summary was built by adding one topic. Stored for performance reasons.
;; - features : functions to compute the feature values for next summaries resulting from the current one by adding a topic.
;; - feature-vals : pre-computed feature values for the current summary. Stored for performance reasons.
;; TopicSumbap is a state (IState) in the process of sequential learning
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

(defn empty-submap [topic-map features]
  (let [submap (tmaps/submap topic-map [] :keep [:main-topic])]
    (TopicSubmap. topic-map []
                  submap 
                  nil
                  features
                  (mapv #(%1 topic-map) features))))

(defn get-init-state [topic-submap]
  (->> topic-submap
       (iterate sl/previous-state)
       (take-while (complement nil?))
       last))

;; Loss function used for comparing actions to the expert action.
(defrecord ActionLoss [loss-fn]
  da/IActionLoss
  (compute-action-loss [this state action1 action2]
    (loss-fn state action1 action2)))


;; Loss function used when computing expert actions in DAgger.
(defrecord StateActionLoss [loss-fn]
  da/IStateActionLoss
  (compute-state-action-loss [this optimal-state given-state action]
    (loss-fn optimal-state given-state action)))

(defn zero-one [a b] (if (= a b) 0 1))

(def action-loss-01 (ActionLoss. (fn [state action1 action2] (zero-one action1 action2))))

(defn match-score
  "Computes the 'similarity' metween two topic sequences by greedily assigning
   best-matching topics from the predicted sequence to the topics in the optimal sequence.
   Similarity between the topics is based on the documents they contain,
   and is computed as the Jaccard index between the sets of documents,
   + alpha=0.1 if the topics are the same, divided by (1 + alpha)."
  [topic-map optimal-topics predicted-topics]
  (let [topic-graph (:topic-graph topic-map)
        ;; start костыль
        title-topic-map (u/key-map :title (tmaps/get-topics topic-map))
        predicted-topics (map (comp title-topic-map :title) predicted-topics)
        optimal-topics (map (comp title-topic-map :title) optimal-topics)
        ;; end костыль
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
                     (apply max-key (memoize (partial topic-similarity topic)) topics))
;        _ (println "Optimal: " optimal-topics)
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

(def smart-matching-loss
  "The loss function for an action given a current non-optimal state,
   and the corresponding optimal state. 
   Computes one minus the match-score between the corresponding topic sequences."
  (StateActionLoss. (fn smart-matching-loss [optimal-state given-state action]
           (let [topic-map (:topic-map optimal-state)
                 optimal-topics (:topics optimal-state)
                 predicted-topics (conj (:topics given-state) action)]
             (- 1 (match-score topic-map optimal-topics predicted-topics))))))

(defn read-ground-truth
  "Reads the topic map for the query, along with the 'ground truth' action(topic) sequence.
   Returns the pair [zeroth-state action-seq]."
  [conf query]
  (let [topic-map (->> (input-file-name conf query) slurp read-string tmaps/cache-merged #_ tmaps/cache-topic-dist)
        {:keys [topic-graph]} topic-map
        topic-names (read-string (slurp (output-file-name conf query)))
        topic-name-map (u/key-map :title (g/get-nodes topic-graph))
        topic-by-name #(or (topic-name-map %) (throw (Exception. (str "No topic for name: " %))))
        topics (mapv topic-by-name topic-names)]
    [(empty-submap topic-map (conf :features)) topics]))

(defn state-action-seqs
  "Reads the topic maps and 'ground truth' action(topic) sequences for the given queries.
   Returns the [zeroth-state topic-seq] pairs."
  [conf queries]
  (map (partial read-ground-truth conf) queries))

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

(defn train-dagger
  ([conf queries seq-length n-iter & {:keys [evaluate]}]
     (let [input-data (state-action-seqs conf queries)]
       (da/dagger input-data #_ seq-length
                  #_ topic-overlap-loss
                  smart-matching-loss
                  action-loss-01
                  n-iter
                  :evaluate evaluate))))

(defn train-dagger* [input-data n-iter]
  (da/dagger input-data
             smart-matching-loss
             action-loss-01
             n-iter))

(defn run-fold [queries features ntopics niter fold]
  (let [query (get queries fold)]
    (train-dagger (remove #{query} queries) features ntopics niter :evaluate #(accuracies % query features))))

(defn leave-one-out [queries features ntopics niter]
  (doseq [query queries]
    (train-dagger (remove #{query} queries) features ntopics niter :evaluate #(accuracies % query features))))

(defn predict-compare [conf model query n]
  (let [[init-state optimal-seq] (read-ground-truth conf query)
        topic-map (:topic-map init-state)
        predicted-seq (sl/best-action-seq model init-state n)]
    (println "Optimal sequence:" (map :title optimal-seq))
    (println "Predicted sequence:" (map :title predicted-seq))
 #_ (tmaps/display-topics topic-map)
 #_ (tmaps/display-topics (tmaps/submap topic-map optimal-seq))
 (tmaps/display-topics (tmaps/submap topic-map predicted-seq))))

;; (defn assess-baseline [query features]
;;   (let [[init-state optimal-seq] (read-ground-truth query features)
;;         topic-map (:topic-map init-state)
;;         n (count optimal-seq)
;;         all-topics (tmaps/get-topics topic-map)
;;         iter (fn [predicted-seq covered-docs]
;;                (if (= n (count predicted-seq))
;;                  predicted-seq
;;                  (let [;covered-docs (set (mapcat #(proper-docs topic-map %)))
;;                        candidate-topics (remove (set predicted-seq) all-topics)
;;                        docs-covered-with (u/val-map #(into covered-docs (tmaps/proper-docs topic-map %)) candidate-topics)
;;                        score-fn (comp count docs-covered-with)
;;                        next-topic (apply max-key score-fn candidate-topics)]
;;                    (recur (conj predicted-seq next-topic) (docs-covered-with next-topic)))))
;;         predicted-seq (iter [] #{})]
;;     (println "Optimal sequence:" (map :title optimal-seq))
;;     (println "Predicted sequence:" (map :title predicted-seq))
;;     (println "Accuracies: " (accuracies-at optimal-seq predicted-seq))
;;     (println "Match-score: " (match-score-at topic-map optimal-seq predicted-seq))
;;     (tmaps/display-topics (tmaps/submap topic-map optimal-seq))
;;     (tmaps/display-topics (tmaps/submap topic-map predicted-seq))))

(defn display-topics [conf query topic-names]
  (let [[init-state optimal-seq] (read-ground-truth conf query)
        topic-map (:topic-map init-state)
        topic-names (set topic-names)
        all-topics (tmaps/get-topics topic-map)
        topics (filter (comp topic-names :title) all-topics)
        submap (tmaps/submap topic-map topics)]
    (tmaps/display-topics submap)))

(defn -main []
  (let [app (new-app :input-data-dir "./resources/data-aminer-wminer-2013-10-07/" :mk-doc mk-paper)
        conf (:conf app)
        queries (:queries (:data app))]
    (train-dagger conf queries 8 10)))
