(ns dot-api.core
  (:require [clojure [string :as string]])
  (:use [clojure.java.shell :only [sh]]))

(def avail-algos
  #{:dot :neato :twopi :circo :fdp :sfdp})

(defn dotstr2svg [dotstring algo]
  {:pre [(avail-algos algo)]}
  (:out (sh (name algo) "-Tsvg" :in dotstring)))

(defn show-svg [svg]
  (let [file (java.io.File/createTempFile "smth" ".svg")]
    (spit file svg)
    (sh "open" (.getAbsolutePath file))))
