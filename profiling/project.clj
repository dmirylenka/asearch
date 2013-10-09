(defproject profiling "0.0.1"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [unitn/utils "0.0.1"]
                 [unitn/mas-api "0.0.1"]
                 [unitn/arnetminer "0.0.1"]
                 [unitn/arxiv-api "0.0.1"]
                 [unitn/search-api "0.0.1"]
                 [unitn/wiki-api "0.0.1"]
                 [unitn/wminer "0.0.1"]
                 [unitn/wminer-api "0.0.1"]
                 [unitn/mlinking "0.0.1"]
                 [unitn/topic-maps "0.0.1"]
                 [incanter "1.5.1"]]
  :main profiling.profiling
  :jvm-opts ["-Xmx3g" "-server"])
