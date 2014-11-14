(ns scienscan.conf)

(def conf 
  {:mas-app-id ""
   :aminer-opt {:db {:subprotocol "mysql"
                     :subname "//localhost:3306/msdataset"
                     :user "dmirylenka"
                     :password "dmirylenka"}
                :user "dmirylenka"
                :fail-empty true}
   :scienscan-opt {:n-topics 5
                   :n-labels 8
                   :n-candidates 5
                   :n-dagger-iter 10
                   :dagger-model "/Users/dmirylenka/code/asearch-modular/learn-submap/resources/models/aminer-wminer-10q-13-10-09.model"
                   :query "statistical relational learning"
                   :search-timeout 30000
                   :n-results 100}})
