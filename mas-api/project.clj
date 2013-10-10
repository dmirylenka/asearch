(defproject unitn/mas-api "0.0.1"
  :description "A wrapper for the search service of Microsoft Academic Search."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clj-http "0.7.0"]
                 [unitn/utils "0.0.1"]
                 [unitn/search-api "0.0.1"]]
  :main mas-api.core)
