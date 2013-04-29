(ns seq-learn.dagger
  (:use [seq-learn.core]))

; dataset: [init. states] [local decision examples]
; run policy on init. states -> sequences of states
; transform sequences into local decision examples using optimal policy
; add examples to the dataset
; train new policy

(defn compute-state-seq* [init-state model seq-length]
  (letfn [(best-next [state]
            (next-state (best-action model state) state))
          (iter [states]
            (if (= seq-length (count states))
              states
              (recur (conj states (best-next (peek states))))))]
    (iter [init-state])))
        

; init-states - x + empty actions
; action-seqs - sequences of actions of current policy on initial states
; state-seqs - sequences of states 

(defn- iterate-dagger [init-states opt-policy seq-length loss feature-fn {:keys [dataset current-policy]}]
  (println "!!!!!!!!!!!!!!!!!!!!!!!!!!!!! NEW ITERATION !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  (let [state-seqs (mapcat #(compute-state-seq* % current-policy seq-length) init-states)
        action-seqs (map #(best-action opt-policy %) state-seqs)
        new-data (map vector state-seqs action-seqs)
        _ (doseq [[state action] new-data]
            (println (clojure.string/join ", " (map :title (:topics state))) "->" (action-name action)))
        dataset (concat dataset new-data)]
    {:dataset dataset
     :current-policy (train-model* dataset loss feature-fn :c 3200)}))
;
;(defn- mk-ranking [state-action-seqs loss feature-fn]
;  (let [compute-optimal-states (fn [[optimal-state optimal-action-seq]]
;                                 (reductions #(next-state %2 %1) optimal-state optimal-action-seq))
;        optimal-states (mapcat compute-optimal-states state-action-seqs)
;        optimal-actions (mapcat second state-action-seqs)
;        rank-value (fn rank-value [state optimal-next-state action]
;                     (- (compute-loss loss (next-state action state) optimal-next-state)))
;        out-rank (fn out-rank [query-id state optimal-next action]
;                   (OutputRank. (Query. query-id state) action (rank-value state optimal-next action) feature-fn))]
;    (vec
;      (for [[state optimal-action query-id] (map vector optimal-states optimal-actions (rest (range)))
;            :let [optimal-next-state (next-state optimal-action state)]
;            action (next-actions state)]
;        (out-rank query-id state optimal-next-state action)))))

#_(defn actions->state-map [[init-state action-seq]]
  (let [state-seq (compute-state-seq init-state action-seq)]
    (into {} (partition 2 1 state-seq))))

(defrecord MinLossAction [action state]
  IAction
  (action-name [this] (str "min-loss: " (action-name action)))
  (-next-state [this state*] 
    state))

(defn min-loss-action [state desired-state loss]
  (let [actions (next-actions state)
        states (map #(next-state % state) actions) 
        loss-value #(compute-loss loss desired-state (second %))
        best-action-state (apply min-key loss-value (map vector actions states))] ; don't compute actions, just the optimal state 
  ; (println (map :title (:topics state)))
  ; (println (map :title (:topics desired-state)))
  ; (println (:title (first best-action-state)))
    (apply ->MinLossAction best-action-state))) ; pass just the optimal state 

(defrecord OptModel [action-map loss]
  IModel
  (best-action [this state]
    (let [iter (fn [state n]
                 (let [prev-state (previous-state state)]
                   (if-not prev-state 
                     [state n]
                     (recur prev-state (inc n)))))
          [init-state n] (iter state 1)
          opt-state (reduce #(next-state %2 %1) init-state (take n (action-map init-state)))]
;     (println "cur:" (map :title (:topics state)))
;     (println "init:" (map :title (:topics init-state)))
;     (println "opt:"(map :title (:topics opt-state)))
      (min-loss-action state opt-state loss))))

(defn mk-opt-policy [state-action-seqs loss feature-fn]
  (let [state-action-map (into {} state-action-seqs)]
    (->OptModel state-action-map loss)))

;(defn- iterate-dagger [init-states opt-policy seq-length loss feature-fn {:keys [dataset current-policy]}]

(defn dagger [state-action-seqs seq-length loss feature-fn n-iter]
  (let [init-states (map first state-action-seqs)
        opt-policy (mk-opt-policy state-action-seqs loss feature-fn)
        iter (fn iter [{:as model :keys [dataset current-policy]} cnt]
               (if (zero? cnt) current-policy
                 (recur (iterate-dagger init-states opt-policy seq-length loss feature-fn model) (dec cnt))))]
    (iter {:dataset [] :current-policy opt-policy} n-iter)))

;(defprotocol IState
;  (state-name [this])
;  (next-actions [this]))
;
;(defprotocol IAction
;  (action-name [this])
;  (-next-state [this state]))
;
;(defprotocol ILoss
;  (compute-loss [this state1 state2]))
;
;(defprotocol IFeatures
;  (-features [this state]))
;
;(def features (memoize #'-features))
;
;(def next-state (memoize #'-next-state))
;
;(defprotocol IModel
;  (best-action [this state]))
;
;(defrecord Model [svm-model feature-fn]
;  IModel
;  (best-action [this state]
;    (let [actions (next-actions state)
;          query-id 1
;          query (Query. query-id state)
;          ranking (mapv #(OutputRank. query % 0 feature-fn) actions)
;          ranking (svm-rank/predict-ranking svm-model ranking)]
;      (->> ranking (sort-by (comp - :rank)) first :action))))
;
;(defn best-action-seq [model state n]
;  (let [next-action #(best-action model %)
;        iter (fn iter [state actions]
;               (if (= n (count actions))
;                 actions
;                 (let [action (next-action state)]
;                   (recur (next-state action state) (conj actions action)))))]
;    (iter state [])))
;
;(defn- mk-ranking [state-action-seqs loss feature-fn]
;  (let [compute-optimal-states (fn [[optimal-state optimal-action-seq]]
;                                 (reductions #(next-state %2 %1) optimal-state optimal-action-seq))
;        optimal-states (mapcat compute-optimal-states state-action-seqs)
;        optimal-actions (mapcat second state-action-seqs)
;        rank-value (fn rank-value [state optimal-next-state action]
;                     (- (compute-loss loss (next-state action state) optimal-next-state)))
;        out-rank (fn out-rank [query-id state optimal-next action]
;                   (OutputRank. (Query. query-id state) action (rank-value state optimal-next action) feature-fn))]
;    (vec
;      (for [[state optimal-action query-id] (map vector optimal-states optimal-actions (rest (range)))
;            :let [optimal-next-state (next-state optimal-action state)]
;            action (next-actions state)]
;        (out-rank query-id state optimal-next-state action)))))
;
;(defn train-model [state-action-seqs loss feature-fn & {:as opt}]
;  (let [ranking (mk-ranking state-action-seqs loss feature-fn)
;        svm-model (svm-rank/train-model ranking :c (:c opt))]
;    (Model. svm-model feature-fn)))
