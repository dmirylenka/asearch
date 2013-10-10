(defproject unitn/arxiv-api "0.0.1"
  :description "A wrapper for the search service of arXiv."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clj-http "0.7.0"]
                 [org.clojure/data.xml "0.0.7"]
                 [org.clojure/data.zip "0.1.1"]
                 [unitn/utils "0.0.1"]]
  :main arxiv-api.core)
