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

(defrecord OutputRank [query action rank feature-fn]
  svm-rank/IRank
  (get-query [this] query)
  (output-features [this]
    (features feature-fn (next-state action (:state query))))
  (get-value [this] rank)
  (assoc-value [this value]
    (assoc this :rank value))
  (output-info [this]
    (action-name action)))

(defprotocol IModel
  (best-action [this state]))

(defrecord Model [svm-model feature-fn]
  IModel
  (best-action [this state]
    (let [actions (next-actions state)
          query-id 1
          query (Query. query-id state)
          ranking (mapv #(OutputRank. query % 0 feature-fn) actions)
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
                   (OutputRank. (Query. query-id state) action (rank-value state optimal-next action) feature-fn))]
    (vec
      (for [[state optimal-action query-id] (map vector optimal-states optimal-actions (rest (range)))
            :let [optimal-next-state (next-state optimal-action state)]
            action (next-actions state)]
        (out-rank query-id state optimal-next-state action)))))

(defn produce-ranking [state-action-seq loss feature-fn]
  (let [rank-value (fn rank-value [state optimal-next-state action] ; take loss-aware-action instead of optimal-next
                     (- (compute-loss loss (next-state action state) optimal-next-state))) ; compute loss with loss-aware-action 
        out-rank (fn out-rank [query-id state optimal-next action] ; take loss-aware-action instead of optimal-next
                   (OutputRank. (Query. query-id state) action (rank-value state optimal-next action) feature-fn))] ; pass loss-aware-action instead of optimal-next
    (vec
      (for [[query-id [state optimal-action]] (zipmap (rest (range)) state-action-seq) ; change optimal-action to loss-aware action
            :let [optimal-next-state (next-state optimal-action state)] ; don't compute next state
            action (next-actions state)]
        (out-rank query-id state optimal-next-state action))))) ; instead of optimal-next-state, pass loss-aware-action

(defn train-model* [state-action-seq loss feature-fn & {:as opt}]
  (let [ranking (produce-ranking state-action-seq loss feature-fn)
        svm-model (svm-rank/train-model ranking :c (:c opt))]
    (Model. svm-model feature-fn)))

(defn train-model [state-action-seqs loss feature-fn & {:as opt}]
  (let [ranking (mk-ranking state-action-seqs loss feature-fn)
        svm-model (svm-rank/train-model ranking :c (:c opt))]
    (Model. svm-model feature-fn)))
