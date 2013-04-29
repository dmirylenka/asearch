(ns seq-learn.dagger
  (:use [seq-learn.core])
  (:require [topic-maps [core :as tmaps]]
            [utils [core :as u]]))

(defprotocol IOptModel
  (optimal-next [this action]))

(defn compute-states [init-state model seq-length]
  (let [best-next (fn best-next [state]
                    (next-state (best-action model state) state))
        iter (fn iter [states]
               (if (= seq-length (count states))
                 states
                 (recur (conj states (best-next (peek states))))))
        state-seq (iter [init-state])]
    ;(tmaps/display-topics (tmaps/submap (:topic-map (last state-seq)) (:topics (last state-seq))))
    state-seq))

(defn- iterate-dagger [init-states opt-policy seq-length loss feature-fn {:keys [dataset current-policy evaluate]}]
 ; (println "!!!!!!!!!!!!!!!!!!!!!!!!!!!!! NEW ITERATION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  (let [states (mapcat #(compute-states % current-policy seq-length) init-states)
        new-states ;(remove (set (map first dataset))
          states;)
        ;_ (println (count new-states) "new states")
        new-data (for [state new-states
                       :let [optimal-state (optimal-next opt-policy state)] 
                       action (next-actions state)
                       :let [next-st (next-state action state)]]
                   [state next-st (compute-loss loss next-st optimal-state)])
        min-losses (->> new-data (u/group-map first last) (u/map-val (partial apply min))) 
        new-data (for [[state next-st loss-value] new-data]
                   [state next-st (if (= loss-value (min-losses state)) 0 1)])
  ;   _ (doseq [[state info] (group-by first new-data)
  ;           :let [best-next-st (second (apply min-key last info))]
  ;           :when (= seq-length (count (:topics best-next-st)))]
  ;       (println (clojure.string/join ", " (map :title (:topics best-next-st)))))
        dataset (concat dataset new-data)
        new-model (train-model* dataset feature-fn)]
    (when evaluate (evaluate new-model))
    {:dataset dataset
     :current-policy new-model
     :evaluate evaluate}))

(defrecord StateAction [state]
  IAction
  (action-name [this] (state-name state))
  (-next-state [this action]
    state))

(defrecord OptModel [action-map loss]
  IModel
  (best-action [this state]
    (StateAction. (optimal-next this state)))
  IOptModel
  (optimal-next [this state]
    (let [iter (fn [state n]
                 (let [prev-state (previous-state state)]
                   (if-not prev-state 
                     [state n]
                     (recur prev-state (inc n)))))
          [init-state n] (iter state 1)]
      (reduce #(next-state %2 %1) init-state (take n (action-map init-state))))))

(defn mk-opt-policy [state-action-seqs loss feature-fn]
  (let [state-action-map (into {} state-action-seqs)]
    (->OptModel state-action-map loss)))

(defn dagger [state-action-seqs seq-length loss feature-fn n-iter & {:keys [evaluate]}]
  (let [init-states (map first state-action-seqs)
        opt-policy (mk-opt-policy state-action-seqs loss feature-fn)
        iter (fn iter [{:as model :keys [dataset current-policy]} cnt]
               (if (zero? cnt) current-policy
                 (recur (iterate-dagger init-states opt-policy seq-length loss feature-fn model) (dec cnt))))]
    (iter {:dataset [] :current-policy opt-policy :evaluate evaluate} n-iter)))
