(ns scienscan.conf)

(def conf 
  {:mas-app-id ""
   :aminer-opt {:db {:subprotocol "mysql"
                     :subname "//localhost:3306/aminer_dataset"
                     :user "root"
                     :password ""}
                :user ""
                :fail-empty true}
   :scienscan-opt {:n-topics 5
                   :n-labels 8
                   :n-candidates 5
                   :n-dagger-iter 10
                   :query "statistical relational learning"
                   :search-timeout 30000
                   :n-results 100}})
