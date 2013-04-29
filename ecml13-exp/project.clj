(defproject unitn/ecml13-exp "0.0.1"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [unitn/utils "0.0.1"]
                 [unitn/mas-api "0.0.1"]
                 [unitn/topic-maps "0.0.1"]
                 [unitn/seq-learn "0.0.1"]
;                 [unitn/svm-rank "0.0.1"]
                 ]
  :main ecml13-exp.core
  :jvm-opts ["-Xmx2g" "-server"])
