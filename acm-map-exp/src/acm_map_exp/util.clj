(ns acm-map-exp.util
  (:refer-clojure :exclude [rand-nth]))

(defn new-rand-int-gen
  "Returns a random integer generator - a function that returns random integers.
   May accept a random seed as a parameter."
  ([] rand-int)
  ([random-seed]
     (let [rand-int-gen (java.util.Random. random-seed)]
       (fn new-rand-int [n]
         (.nextInt rand-int-gen n)))))

(defn rand-nth
  "Like clojure.core/rand-int, only may accepts a random integer generator."
  ([coll]
     (rand-nth (seq coll) clojure.core/rand-int))
  ([coll rand-int-gen]
     (nth (seq coll) (rand-int-gen (count coll)))))

;; (defn rand-take
;;   "Attempts to select k random distinct elements from the collection.
;;    Does not use shuffle, but selects random elements repeatedly.
;;    Retries the selection 10 times or until all elements are distinct.
;;    Throws an exception if does not succeed from the 10 attempts.
;;    Accepts a random integer generator as an optional argument."
;;   ([k coll]
;;      (rand-take k coll (new-rand-int-gen)))
;;   ([k coll rand-int-gen]
;;      (letfn [(attempt [k coll n-attempts attempts-left]
;;                (if (zero? attempts-left)
;;                  (throw (Exception. (str "Could not select " k " distinct random elements from the collection of size " (count coll) " from " n-attempts " attempts.")))
;;                  (let [result (distinct (repeatedly k #(rand-nth coll rand-int-gen)))]
;;                    (if (= k (count result))
;;                      result
;;                      (recur k coll n-attempts (dec attempts-left))))))]
;;        (attempt k coll 10 10))))

(defn rand-take
  ([k coll]
     (rand-take k coll (new-rand-int-gen)))
  ([k coll rand-int-gen]
     (letfn [(iter [result elements-left]
               (if (or (= k (count result))
                       (empty? elements-left))
                 result
                 (let [new-el (rand-nth elements-left rand-int-gen)]
                   (recur (conj result new-el) (disj elements-left new-el)))))]
       (iter '() (set coll)))))


(defn map-key [f coll]
  (into {} (map (fn [[k v]] [(f k) v]) coll)))

(defn map-val [f coll]
  (into {} (map (fn [[k v]] [k (f v)]) coll)))

(defn map-kv [f coll]
  (into {} (map (fn [[k v]] [k (f k v)]) coll)))

(defn group-map [fn1 fn2 coll]
  (->> coll
    (group-by fn1)
    (map-val (partial map fn2))))

(defn val-map [f coll]
  (into {} (for [item coll
                 :let [value (f item)]
                 :when value]
             [item value])))

(defn key-map [f coll]
  (into {} (for [item coll
                 :let [key (f item)]
                 :when key]
             [key item])))

(defn filter-key [f m]
  (into {} (filter (fn [[k v]] (f k)) m)))

(defn filter-val [f m]
  (into {} (filter (fn [[k v]] (f v)) m)))

(defn wrap
  ([field-name string]
    {field-name string})
  ([field-name]
    (fn [string]
      {field-name string})))

(defn assocf [obj f g field]
  (if (not (map? obj))
    (assocf {g obj} f g field)
    (assoc obj field (f (g obj)))))
