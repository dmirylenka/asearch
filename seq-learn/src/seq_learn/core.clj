(ns seq-learn.core
  (:require [utils [core :as u]]
            [svm-rank [core :as svm-rank]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Protocols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defprotocol IState
  (previous-state [this])
  (state-name [this])
  (next-actions [this])
  (next-state [this action]))

(defprotocol IAction
  (action-name [this]))

(defprotocol IActionFeatures
  (compute-features [this] [this action]))

(defprotocol IModel
  (best-action [this state])
  (save-model [this file-name]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Implementation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defrecord StateQuery [query-id state]
  svm-rank/IQuery
  (query-id [this] query-id)
  (query-info [this] (state-name state)))

(defrecord ActionRank [qid state action rank]
  svm-rank/IRank
  (rank-query [this] (StateQuery. qid state))
  (output-features [this]
    (compute-features state action))
  (rank-value [this] rank)
  (assoc-value [this value]
    (assoc this :rank value))
  (output-info [this]
    (action-name action)))

(defrecord SvmRankModel [svm-model]
  IModel
  (best-action [this state]
    (let [QID 1
          RANK 0
          actions (next-actions state)
          ranking (mapv #(ActionRank. QID state % RANK) actions)
          ranking (svm-rank/predict-ranking svm-model ranking)]
      (->> ranking (sort-by (comp - :rank)) first :action)))
  (save-model [this file-name]
    (svm-rank/save-model svm-model file-name))) 

(defn load-model [file]
  (SvmRankModel. (svm-rank/load-model file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defn best-action-seq [model state n]
  (let [next-action #(best-action model %)
        iter (fn iter [state actions]
               (if (= n (count actions))
                 actions
                 (let [action (next-action state)]
                   (recur (next-state state action) (conj actions action)))))]
    (iter state [])))

(defn- produce-ranking [transition-loss-seq]
  (let [id-transition-loss-seq (map vector (partition-by first transition-loss-seq) (rest (range)))]
    (vec (for [[state-action-loss-seq qid] id-transition-loss-seq
               [state action loss-value] state-action-loss-seq]
           (ActionRank. qid state action (- loss-value))))))

(defn train-model [transition-loss-seq & {:as opt}]
  (let [ranking (produce-ranking transition-loss-seq)
        svm-model (svm-rank/train-model ranking :c (:c opt))]
    (SvmRankModel. svm-model)))
