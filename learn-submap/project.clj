(defproject unitn/learn-submap "0.0.1"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [unitn/utils "0.0.1"]
                 [unitn/wiki-api "0.0.1"]
                 [unitn/topic-maps "0.0.1"]
                 [unitn/seq-learn "0.0.1"]
                 [unitn/mas-api "0.0.1"]]
  :main learn-submap.lrn-sbm
  :jvm-opts ["-Xmx2g" "-server"])
