(defproject unitn/cikm13-exp "0.0.1"
  :description "Experiments for CIKM2013 conference."
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [unitn/utils "0.0.1"]
                 [unitn/mas-api "0.0.1"]
                 [unitn/topic-maps "0.0.1"]
                 [unitn/seq-learn "0.0.1"]]
  :main cikm13-exp.core
  :jvm-opts ["-Xmx2g" "-server"])
