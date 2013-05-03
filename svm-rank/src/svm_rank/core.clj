(ns svm-rank.core
  (:require [clojure.java [io :as io]]
            [clojure [string :as string]])
  (:use [clojure.java.shell :only [sh]]))

(defprotocol IQuery
  (query-id [this])
  (query-info [this]))

(defprotocol IRank
  (rank-query [this])
  (output-features [this])
  (rank-value [this])
  (assoc-value [this value])
  (output-info [this]))

(defprotocol IModel
  (predict-ranking [this rankings]))

(def output-features* (memoize #'output-features))

(def ^:private path (memfn getAbsolutePath))

(defn write-file! [ranking file-name]
  (let [sorted-ranking (sort-by (comp query-id rank-query) ranking)]
    (with-open [w (io/writer file-name)]
      (doseq [rank sorted-ranking]
        (let [features (output-features rank)
              query (rank-query rank)
              comment (str "# " (query-info query) " -> " (output-info rank))]
          (.write w (str (* 1.0 (rank-value rank)) " qid:" (query-id query) " "))
          (doseq [i (range (count features))]
            (.write w (str (inc i) ":" (* 1.0 (:value (nth features i))) " ")))
          (.write w comment)
          (.newLine w))))
    file-name))

(defn temp-file [ext]
  (java.io.File/createTempFile "smth" (str "." ext)))

(def svm-path "/Users/dmirylenka/pkgs/svm-rank/")

(defn run-svm-train! [input-file model-file & {:keys [c] :or {c 1}}]
 ; (println "-c " c)
 ; (println (:out 
           (sh (str svm-path "svm_rank_learn") "-c" (str c)
              ; "-e" "0.001"
               (path input-file) (path model-file))
 ;          ))
  model-file)

(defn run-svm-predict! [input-file output-file model-file]
  (sh (str svm-path "svm_rank_classify") (path input-file) (path model-file) (path output-file))
  output-file)

(defrecord Model [model-file]
  IModel
  (predict-ranking [this rankings]
    (let [input-file (temp-file "test")
          output-file (temp-file "predict")
    ;     _ (println (path model-file))
    ;     _ (println (path input-file))
    ;     _ (println (path output-file))
          input-file (write-file! rankings input-file)
          output-file (run-svm-predict! input-file output-file model-file)
          rank-values (->> output-file slurp (#(string/split % #"\n")) (map #(Double/parseDouble %)))]
      (mapv assoc-value rankings rank-values))))

(defn train-model [ranking & {:as opt}]
  (let [input-file (or (io/file (:input-file opt)) (temp-file "train"))
        model-file (or (io/file (:model-file opt)) (temp-file "model"))
    ;   _ (println (path input-file))
        _ (println (path model-file))
        input-file (write-file! ranking input-file)
;       _ (println "Input file for training SVM is written.")
        nqueries (count (distinct (mapv (comp query-id rank-query) ranking)))
        model-file (run-svm-train! input-file model-file :c (or (:c opt) nqueries))]
    (Model. model-file)))
