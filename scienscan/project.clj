(defproject unitn/scienscan "0.0.1"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [compojure "1.1.5"]
                 [lib-noir "0.6.4"]
                 [unitn/utils "0.0.1"]
                 [unitn/mas-api "0.0.1"]
                 [unitn/topic-maps "0.0.1"]
                 [unitn/learn-submap "0.0.1"]]
  :plugins [[lein-ring "0.8.5"]]
  :ring {:handler scienscan.handler/app}
  :main scienscan.handler
  :jvm-opts ["-Xmx3g" "-server"]
  :profiles
  {:dev {:dependencies [[ring-mock "0.1.5"]]}})
