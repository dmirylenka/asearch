(defproject unitn/taxonorous "0.0.1"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.namespace "0.2.3"]
                 [org.clojure/data.zip "0.1.1"]
                 [unitn/utils "0.0.1"]
                 [unitn/wiki-api "0.0.1"]
                 [unitn/wminer "0.0.1"]]
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/tools.namespace "0.2.3"]]}}
  :main taxonorous.txn)
