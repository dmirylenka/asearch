(defproject unitn/learn-submap "0.0.1"
  :description "A project for learning the topic map from 'ground truth' summaries of the topic graphs."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [unitn/utils "0.0.1"]
                 [unitn/wiki-api "0.0.1"]
                 [unitn/topic-maps "0.0.1"]
                 [unitn/seq-learn "0.0.1"]
                 [unitn/search-api "0.0.1"]
                 [unitn/mas-api "0.0.1"]
                 [unitn/arxiv-api "0.0.1"]
                 [unitn/arnetminer "0.0.1"]
                 [org.clojure/java.jdbc "0.3.0-alpha5"]
                 [mysql/mysql-connector-java "5.1.26"]]
  :main learn-submap.database
  :jvm-opts ["-Xmx2g" "-server"])
