(defproject unitn/arnetminer "0.0.1"
  :description "A wrapper for the search service of arnetminer."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [clj-http "0.7.0"]
                 [unitn/utils "0.0.1"]
                 [unitn/search-api "0.0.1"]
                 [org.clojure/java.jdbc "0.3.0-alpha5"]
                 [mysql/mysql-connector-java "5.1.26"]]
  :main arnetminer.dataset)
