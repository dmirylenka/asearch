(ns utils.core 
  (:import (java.util.concurrent TimeoutException TimeUnit FutureTask))
  (:require (clojure [string :as string])))

(defn round
  ([number] (round number 2))
  ([number ndigits]
   (let [power-of-ten (Math/pow 10 ndigits)]
     (-> number
       (* power-of-ten)
       Math/round
       (/ power-of-ten)))))

(defn avg [arr]
  (/ (apply + arr) (count arr)))

(defn extract [s] (string/lower-case (string/replace s #"\W" "")))

(defn strip-digits [s] (string/replace s #"\d" ""))

(defn extract* [s] (strip-digits (extract s)))

(defn ungroup [coll]
  (into {} (for [[v ks] coll k ks] [k v])))

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

(defn replace-acronyms
  ([string] (replace-acronyms string true))
  ([string inside-acronym]
    (if (empty? string) ""
      (let [[first-char & rest-string] string
            isCap (Character/isUpperCase first-char)]
        (if inside-acronym
          (str (string/lower-case first-char) (replace-acronyms rest-string isCap)) 
          (str first-char (replace-acronyms rest-string isCap)))))))

(defn cap-to-dash [string]
  (let [first-char (first string)
        rest-string (rest string)]
  (apply str (string/lower-case first-char)
    (for [c rest-string]
      (if (Character/isUpperCase c)
        (str "-" (string/lower-case c))
        c)))))

(defn dash-to-cap [string]
  (let [[dash-c c] (re-find #"-(.)" string)]
    (if-not c string
      (recur (string/replace string dash-c (string/upper-case c))))))

(defn apply-kw [f kw]
  (keyword (f (name kw))))

(defn keys-to-dash [m]
  (cond
    (map? m) (->> m
               (map-key (partial apply-kw (comp cap-to-dash replace-acronyms)))
               (map-val keys-to-dash))
    (coll? m) (map keys-to-dash m)
    :otherwise m))

(def ^:dynamic *random-seed* 69797)

(let [rand-int-gen (java.util.Random. *random-seed*)]
    (defn new-rand-int [n]
        (.nextInt rand-int-gen n)))

(defn rand-nth* [coll]
  (nth coll (new-rand-int (count coll))))

(defn rand-take [k coll]
  (letfn [(attempt [k coll n-attempts attempts-left]
            (if (zero? attempts-left)
              (throw (Exception. (str "Could not select " k " distinct random elements from the collection of size " (count coll) " from " n-attempts " attempts.")))
              (let [result (distinct (repeatedly k #(rand-nth* coll)))]
                (if (= k (count result))
                  result
                  (recur k coll n-attempts (dec attempts-left))))))]
    (attempt k coll 10 10)))

(def ^{:doc "Create a map of pretty keywords to ugly TimeUnits"}
  uglify-time-unit
  (into {} (for [[enum aliases] {TimeUnit/NANOSECONDS [:ns :nanoseconds]
                                 TimeUnit/MICROSECONDS [:us :microseconds]
                                 TimeUnit/MILLISECONDS [:ms :milliseconds]
                                 TimeUnit/SECONDS [:s :sec :seconds]}
                 alias aliases]
             {alias enum})))

(defprotocol Result
  (success? [this])
  (success-value [this])
  (-bind [this result-fn]))

(defn fail? [this]
  (not (success? this)))

(defrecord Success [value]
  Result
  (success-value [this] (:value this))
  (success? [this] true)
  (-bind [this result-fn]
    (result-fn (:value this))))

(defrecord Fail [error]
  Result
  (success-value [this]
    (throw (Exception. (str "Can't get a success value of Failure " (:error this)))))
  (success? [this] false)
  (-bind [this result-fn]
    this))

(extend-type Object
  Result
  (success-value [this] this)
  (success? [this] true)
  (-bind [this result-fn] (result-fn this)))

(extend-type nil
  Result
  (success-value [this] nil)
  (success? [this] false)
  (-bind [this result-fn] (result-fn nil)))

;; (defmulti success? [result]
;;   (instance? utils.core.Success result))

;; (defn fail? [result]
;;   (instance? Fail result))


(defmacro bind
  ([value fn1]
   `(-bind ~value ~fn1))
  ([value fn1 & more-fns]
   `(-bind (-bind ~value ~fn1) (fn [result#] (bind result# ~@more-fns)))))

(defn fmap [result ordinary-fn]
  (-bind (-bind result ordinary-fn) ->Success))

(def ^:dynamic *max-iter* 10000)

(defn fixed-point [f x0]
  (letfn [(iter [xn counter]
            (when (zero? counter)
              (throw (Exception. "Maximum number of iterations exceeded.")))
            (let [xn1 (f xn)]
              (if (= xn xn1)
                  (do #_(println (- *max-iter* counter) " iterations") xn1)
                  (recur xn1 (dec counter)))))]
    (iter x0 *max-iter*)))

(defn timeout
  [time func]
  (fn [& args]
    (let [task (FutureTask. #(apply func args))
          thr (Thread. task)]
      (try
        (.start thr)
        (let [result (.get task time (or (uglify-time-unit :ms) :ms))]
          (fmap result identity))
        (catch TimeoutException e
          (.cancel task true)
          (.stop thr) 
          (->Fail :timeout))
        (catch Exception e
          (.cancel task true)
          (.stop thr) 
          (->Fail (.getMessage e)))))))

(defn jaccard [coll1 coll2])
