(ns seq-learn.core
  (:require [utils [core :as u]]
            [svm-rank [core :as svm-rank]]))

(defprotocol IState
  (previous-state [this])
  (state-name [this])
  (next-actions [this]))

(defprotocol IAction
  (action-name [this])
  (-next-state [this state]))

(defprotocol ILoss
  (compute-loss [this state1 state2]))

(defprotocol IFeatures
  (-features [this state]))

(def features (memoize #'-features))

(def next-state (memoize #'-next-state))

(defrecord Query [query-id state]
  svm-rank/IQuery
  (query-id [this] query-id)
  (query-info [this] (state-name state)))

(defrecord ActionRank [query action rank feature-fn]
  svm-rank/IRank
  (get-query [this] query)
  (output-features [this]
    (features feature-fn (next-state action (:state query))))
  (get-value [this] rank)
  (assoc-value [this value]
    (assoc this :rank value))
  (output-info [this]
    (action-name action)))

(defrecord StateRank [query next-st rank feature-fn]
  svm-rank/IRank
  (get-query [this] query)
  (output-features [this]
    (features feature-fn next-st))
  (get-value [this] rank)
  (assoc-value [this value]
    (assoc this :rank value))
  (output-info [this]
    (state-name next-st)))

(defprotocol IModel
  (best-action [this state]))

(defrecord Model [svm-model feature-fn]
  IModel
  (best-action [this state]
    (let [actions (next-actions state)
          query-id 1
          query (Query. query-id state)
          ranking (mapv #(ActionRank. query % 0 feature-fn) actions)
          ranking (svm-rank/predict-ranking svm-model ranking)]
      (->> ranking (sort-by (comp - :rank)) first :action))))

(defn best-action-seq [model state n]
  (let [next-action #(best-action model %)
        iter (fn iter [state actions]
               (if (= n (count actions))
                 actions
                 (let [action (next-action state)]
                   (recur (next-state action state) (conj actions action)))))]
    (iter state [])))

(defn compute-state-seq [state action-seq]
  (butlast (reductions #(next-state %2 %1) state action-seq)))

(defn- mk-ranking [state-action-seqs loss feature-fn]
  (let [optimal-states (mapcat (partial apply compute-state-seq) state-action-seqs)
        optimal-actions (mapcat second state-action-seqs)
        rank-value (fn rank-value [state optimal-next-state action]
                     (- (compute-loss loss (next-state action state) optimal-next-state)))
        out-rank (fn out-rank [query-id state optimal-next action]
                   (ActionRank. (Query. query-id state) action (rank-value state optimal-next action) feature-fn))]
    (vec
      (for [[state optimal-action query-id] (map vector optimal-states optimal-actions (rest (range)))
            :let [optimal-next-state (next-state optimal-action state)]
            action (next-actions state)]
        (out-rank query-id state optimal-next-state action)))))

(defn produce-ranking [transition-loss-seq feature-fn]
  (let [id-transition-loss-seq (map vector (partition-by first transition-loss-seq) (rest (range)))]
    (vec (for [[state-seq qid] id-transition-loss-seq
               [state next-st loss-value] state-seq]
           (StateRank. (Query. qid state)
                       next-st
                       (- loss-value)
                       feature-fn)))))

(defn train-model* [transition-loss-seq feature-fn & {:as opt}]
  (let [ranking (produce-ranking transition-loss-seq feature-fn)
        svm-model (svm-rank/train-model ranking :c (:c opt))]
    (Model. svm-model feature-fn)))

(defn train-model [state-action-seqs loss feature-fn & {:as opt}]
  (let [ranking (mk-ranking state-action-seqs loss feature-fn)
        svm-model (svm-rank/train-model ranking :c (:c opt))]
    (Model. svm-model feature-fn)))
