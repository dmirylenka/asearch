(ns seq-learn.dagger
  (:use [seq-learn.core])
  (:require [topic-maps [core :as tmaps]]
            [utils [core :as u]]))

;(defprotocol IOptModel
;  (optimal-next [this action]))

(defprotocol IActionLoss
  (compute-action-loss [this state action1 action2]))

(defprotocol IStateLoss
  (compute-state-loss [this state1 state2]))

(defprotocol IStateActionLoss
  (compute-state-action-loss [this optimal-state given-state action]))

(defn compute-state-seq [init-state model seq-length]
  (let [best-next (fn best-next [state]
                    (next-state state (best-action model state)))
        grow-states (fn grow-states [states]
                      (let [last-state (peek states)]
                        (if (= seq-length (count states))
                          states
                          (recur (conj states (best-next last-state))))))
        state-seq (grow-states [init-state])]
    ;(tmaps/display-topics (tmaps/submap (:topic-map (last state-seq)) (:topics (last state-seq))))
    state-seq))

(defn- iterate-dagger [init-states opt-policy seq-length loss {:keys [dataset current-policy evaluate]}]
  (println "!!!!!!!!!!!!!!!!!!!!!!!!!!!!! NEW ITERATION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  (let [states (mapcat #(compute-state-seq % current-policy seq-length) init-states)
        new-states states 
          ;(remove (set (map first dataset)) states) ; is it how we should aggregate the dataset??
        new-data (for [state new-states
                       :let [optimal-action (best-action opt-policy state)] 
                       action (next-actions state)
                       :let [loss-value (compute-action-loss loss state optimal-action action)]]
                   [state action loss-value])
  ;   _ (doseq [[state info] (group-by first new-data)
  ;           :let [best-next-st (second (apply min-key last info))]
  ;           :when (= seq-length (count (:topics best-next-st)))]
  ;       (println (clojure.string/join ", " (map :title (:topics best-next-st)))))
        dataset (concat dataset new-data)
        new-model (train-model dataset)]
    (when evaluate (evaluate new-model))
    {:dataset dataset
     :current-policy new-model
     :evaluate evaluate}))

(defrecord OptModel [action-map loss]
  IModel
  (best-action [this state]
    (let [iter-prev-state (fn iter-prev-state [state n]
           (let [prev-state (previous-state state)]
             (if-not prev-state 
               [state n]
               (recur prev-state (inc n)))))
          [init-state n] (iter-prev-state state 1)
          ground-truth-next-state (reduce #(next-state %1 %2) init-state (take n (action-map init-state)))
          action-loss #(compute-state-action-loss loss ground-truth-next-state state %)]
      (apply min-key action-loss (next-actions state)))))

(defn mk-opt-policy [state-action-seqs loss]
  (let [state-action-map (into {} state-action-seqs)]
    (->OptModel state-action-map loss)))

(defn dagger [state-action-seqs seq-length gt-loss opt-loss n-iter & {:keys [evaluate]}]
  (let [init-states (map first state-action-seqs)
        opt-policy (mk-opt-policy state-action-seqs gt-loss)
        iter (fn iter [{:as model :keys [dataset current-policy]} cnt]
               (if (zero? cnt) current-policy
                 (recur (iterate-dagger init-states opt-policy seq-length opt-loss model) (dec cnt))))]
    (iter {:dataset [] :current-policy opt-policy :evaluate evaluate} n-iter)))
