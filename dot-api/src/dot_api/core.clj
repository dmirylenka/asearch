(ns dot-api.core
  (:require [clojure [string :as string]])
  (:use [clojure.java.shell :only [sh]]))

(defn dot2svg [dotstring]
  (:out (sh "dot" "-Tsvg" :in dotstring)))

(defn twopi2svg [dotstring]
  (:out (sh "dot" "-Tsvg" :in dotstring)))

(defn show-svg [svg]
  (let [file (java.io.File/createTempFile "smth" ".svg")]
    (spit file svg)
    (sh "open" (.getAbsolutePath file))))
