(defproject unitn/topic-maps "0.0.1"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/java.jdbc "0.1.4"]
                 [mysql/mysql-connector-java "5.1.6"]
                 [unitn/utils "0.0.1"]
                 [unitn/wminer-api "0.0.1"]
                 [unitn/wminer "0.0.1"]
                 [unitn/wikidb "0.0.1"]
                 [unitn/graphs "0.0.1"]
                 [unitn/dot-api "0.0.1"]
                 [unitn/cluster "0.0.1"]]
  :main topic-maps.learn)
