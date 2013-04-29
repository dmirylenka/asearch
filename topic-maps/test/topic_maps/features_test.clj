(ns topic-maps.features-test
  (:use clojure.test
        topic-maps.core
        topic-maps.features
        )
  (:require [graphs [core :as g]]))

(extend-protocol g/IGraphNode
  Long
  (node-id [this] this)
  (node-title [this] this))

(def docs #{:a :b :c :d :e :f :g :h :i :j :k})

(def topics (range 1 21))

(def topic-graph (-> (g/digraph)
                   (g/add-nodes topics)
                   (g/add-links [[1 4] [2 9] [3 9] [4 10] [4 11] [5 12] [6 12] [6 18] [7 13] [7 14] [8 14] [14 15] [11 16] [12 16] [12 17] [12 18] [12 19] [20 19]])))

(def topic-docs (-> (g/digraph)
                  (g/add-nodes [9 10 4 12 16 17 18 19 20 6 13 14 15 8])
                  (g/add-nodes docs)
                  (g/add-links [[9 :a] [9 :k] [10 :a] [10 :k] [16 :a] [4 :b] [17 :b] [12 :c] [12 :d] [18 :d] [18 :e] [18 :f] [18 :g] [19 :g] [20 :g] [6 :h] [19 :i] [13 :i] [15 :i] [14 :i] [8 :j]])))

(def doc-map (zipmap docs docs))

(def topic-map (->TopicMap topic-graph topic-docs doc-map))

(display-topics topic-map)

(def submap-nodes [7 8 14 6 16 11 12 18 19 20])

(def subm (submap topic-map submap-nodes))

(display-topics subm)

(deftest feature-test
  (testing "paper-coverage"
           (is (= (paper-coverage topic-map subm) (/ 10 11))))
  (testing "direct-doc-coverage"
           (is (= (direct-doc-coverage topic-map subm) (/ 10 11))))
  (testing "distant-paper-coverage"
           (is (= (distant-paper-coverage topic-map subm) [(/ 9 11) (/ 1 11)])))
  (testing "topic-coverage"
           (is (= (topic-coverage topic-map subm) (/ 10 20))))
  (testing "avg-topic-freq"
           (is (= (avg-topic-freq topic-map subm) (/ 13 110))))
  (testing "distant-topic-coverage"
           (is (= (distant-topic-coverage topic-map subm) [(/ 10 20) (/ 3 20)]))
           (is (= (distant-topic-coverage topic-map subm :count :freqs) [(/ 13 21) (/ 3 21)])))
  (testing "partition-coef"
           (is (= (partition-coef topic-map subm) (/ 643 1400))))
  (testing "avg-pairwise-dist"
           (is (= (avg-pairwise-dist topic-map subm) (/ (+ (/ 9 2) (/ 10 3) 31) 45))))
  (testing "unevenness"
           (is (= (unevenness topic-map subm) (/ 1 42)))) 
  (testing "subtopic-coverage"
           (is (= (subtopic-coverage topic-map subm) (/ 7 8)))
           (is (= (subtopic-coverage topic-map subm :direct false) (/ 263 360)))) 
  (testing "v-structures"
           (is (= (v-structures topic-map subm) (/ 3 9 8 (/ 1 2)))) 
           (is (= (v-structures topic-map subm :reversed true) (/ 3 9 8 (/ 1 2))))
           (is (= (v-structures topic-map subm :count :nodes) (/ 3 8)))
           (is (= (v-structures topic-map subm :reversed true :count :nodes) (/ 1 8 ))))
  (testing "n-connected"
           (is (= (n-connected topic-map subm) (/ 2 10)))) 
  (testing "n-links"
           (is (= (n-links topic-map subm) 8)))
  (testing "avg-n-adj"
           (is (= (avg-n-adj topic-map subm) (/ 8 5)))
           (is (= (avg-n-adj topic-map subm :mode :child) (/ 8 6))))
  (testing "main-subtopics"
           (is (= (main-subtopics topic-map (assoc subm :main-topic 6)) (/ 5 10))))
  (testing "height"
           (is (= (height topic-map subm) (/ 2 9))))
  (testing "n-small"
           (is (= (n-small topic-map subm) (/ 4 10)))))
