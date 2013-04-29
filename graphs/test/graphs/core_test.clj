(ns graphs.core-test
  (:use clojure.test
        graphs.core))

(def graph-nodes #{1 2 3 4 5 6 7 8 9 10})

(def graph-links #{[1 2] [1 3] [2 4] [2 7] [3 5] [4 1] [4 7] [5 7] [6 8] [6 10] [8 9] [9 6] [10 8]})

(def graph
  (-> (digraph)
    (add-nodes graph-nodes)
    (add-links graph-links)))

(deftest simple-operations
  (testing "get-links positively"
    (is (= (set graph-links) (set (get-links graph)))))
  (testing "add-node"
    (is (= (set (get-nodes (add-node graph 11))) (conj graph-nodes 11)))
    (is (= (set (get-links (add-node graph 11))) graph-links))
    (is (thrown? Exception (add-node graph 1))))
  (testing "add-nodes"
    (is (= (set (get-nodes (add-nodes graph [11 12]))) (into graph-nodes [11 12])))
    (is (= (set (get-links (add-nodes graph [11 12]))) graph-links))
    (is (thrown? Exception (add-nodes graph [11 12 1 13 14]))))
  (testing "remove-node-safe"
    (is (= (set (get-nodes (remove-node-safe graph 4))) (disj graph-nodes 4)))
    (is (= (set (get-links (remove-node-safe graph 4))) (disj graph-links [2 4] [4 1] [4 7])))
    #_(is (thrown? Exception (remove-node-safe graph 11))))
  (testing "remove-nodes-safe"
    (is (= (set (get-nodes (remove-nodes-safe graph [4 7 8]))) (disj graph-nodes 4 7 8)))
    (is (= (set (get-links (remove-nodes-safe graph [4 7 8]))) (disj graph-links [2 4] [4 1] [4 7] [2 7] [5 7] [6 8] [8 9] [10 8])))
    #_(is (thrown? Exception (remove-nodes-safe graph [4 7 11 8 9]))))
  (testing "contains-link"
    (is (= (contains-link graph 3 5) true))
    (is (= (contains-link graph 5 3) false))
    (is (= (contains-link graph 3 11) false)))
  (testing "add-link"
    (is (= (set (get-nodes (add-link graph 4 5))) graph-nodes))
    (is (= (set (get-links (add-link graph 4 5))) (conj graph-links [4 5])))
    (is (thrown? Exception (add-link graph 7 11)))
    (is (thrown? Exception (add-link graph 11 7)))
    (is (thrown? Exception (add-link graph 11 12))))
  (testing "add-links"
    (is (= (set (get-nodes (add-links graph [[4 5] [4 6] [6 5]]))) graph-nodes))
    (is (= (set (get-links (add-links graph [[4 5] [4 6] [6 5]]))) (conj graph-links [4 5] [6 5] [4 6])))
    (is (thrown? Exception (add-links graph [[4 5] [4 11] [4 6] [6 5]])))
    (is (thrown? Exception (add-links graph [[4 5] [4 6] [11 4] [6 5]])))
    (is (thrown? Exception (add-links graph [[4 5] [4 6] [6 5] [11 12]]))))
  (testing "remove-link"
    (is (= (set (get-nodes (remove-link graph 4 1))) graph-nodes))
    (is (= (set (get-links (remove-link graph 4 1))) (disj graph-links [4 1])))
    (is (thrown? Exception (remove-link graph 5 4)))
    (is (thrown? Exception (remove-link graph 5 11)))
    (is (thrown? Exception (remove-link graph 11 5)))
    (is (thrown? Exception (remove-link graph 11 12))))
  (testing "remove-links"
    (is (= (set (get-nodes (remove-links graph [[4 1] [6 8] [9 6]]))) graph-nodes))
    (is (= (set (get-links (remove-links graph [[4 1] [6 8] [9 6]]))) (disj graph-links [9 6] [4 1] [6 8])))
    (is (thrown? Exception (remove-links graph [[4 1] [11 4] [5 4]])))
    (is (thrown? Exception (remove-links graph [[4 1] [4 11] [6 8]])))
    (is (thrown? Exception (remove-links graph [[4 1] [11 4] [6 8]])))
    (is (thrown? Exception (remove-links graph [[11 12] [11 4] [6 8]]))))
  (testing "add-node-safe"
    (is (= (set (get-nodes (add-node-safe graph 11))) (conj graph-nodes 11)))
    (is (= (set (get-links (add-node-safe graph 11))) graph-links)))
  (testing "add-node-safe ignores existing nodes"
    (is (= (set (get-nodes (add-node-safe graph 4))) graph-nodes))
    (is (= (set (get-links (add-node-safe graph 4))) graph-links)))
  (testing "add-nodes-safe adds new and ignores existing nodes"
    (is (= (set (get-nodes (add-nodes-safe graph [4 5 11 12]))) (into graph-nodes [11 12])))
    (is (= (set (get-links (add-nodes-safe graph [4 5 11 12]))) graph-links)))
  (testing "add-link-safe positively"
    (is (= (set (get-nodes (add-link-safe graph 3 8))) graph-nodes))
    (is (= (set (get-links (add-link-safe graph 3 8))) (conj graph-links [3 8]))))
  (testing "add-link-safe ignores existing links"
    (is (= (set (get-nodes (add-link-safe graph 8 9))) graph-nodes))
    (is (= (set (get-links (add-link-safe graph 8 9))) graph-links)))
  (testing "add-link-safe adds new nodes when needed"
    (is (= (set (get-nodes (add-link-safe graph 8 11))) (conj graph-nodes 11)))
    (is (= (set (get-links (add-link-safe graph 8 11))) (conj graph-links [8 11]))))
  (testing "add-links-safe adds new links, ignores existing links and adds new nodes when needed"
    (is (= (set (get-nodes (add-links-safe graph [[3 8] [9 8] [2 7] [11 5]]))) (conj graph-nodes 11)))
    (is (= (set (get-links (add-links-safe graph [[3 8] [9 8] [2 7] [11 5]]))) (conj graph-links [9 8] [3 8] [11 5]))))
  (testing "break loops"
    (is (= (set (get-nodes (break-loops graph))) graph-nodes))
    (is (= (set (get-links (break-loops graph))) (disj graph-links [4 1] [9 6]))))
  (testing "reachable"
    (is (= (set (reachable graph [4])) #{1 2 4 7 3 5}))
    (is (= (set (reachable graph [3])) #{3 5 7}))
    (is (= (set (reachable graph [3] :direction :backward)) #{3 1 4 2}))
    (is (= (set (reachable graph [2 8])) graph-nodes))
    (is (= (set (reachable graph [5 8] :direction :backward)) (disj graph-nodes 7))))
  (testing "merge-nodes"
    (is (= (set (get-nodes (merge-nodes graph 2 4))) (disj graph-nodes 4)))
    (is (= (set (get-links (merge-nodes graph 2 4))) (-> graph-links (disj [2 4] [4 1] [4 7]) (conj [2 1]))))
    (is (= (set (get-nodes (merge-nodes graph 4 2))) (disj graph-nodes 2)))
    (is (= (set (get-links (merge-nodes graph 4 2))) (-> graph-links (disj [2 4] [2 7] [1 2]) (conj [1 4])))))
  (testing "replace-nodes"
    (is (= (set (get-nodes (replace-node graph 5 11))) (-> graph-nodes (disj 5) (conj 11))))
    (is (= (set (get-links (replace-node graph 5 11))) (-> graph-links (disj [3 5] [5 7]) (conj [3 11] [11 7])))))
  (testing "remove-propagate"
    (is (= (set (get-nodes (remove-propagate graph 8))) (-> graph-nodes (disj 8))))
    (is (= (set (get-links (remove-propagate graph 8))) (-> graph-links (disj [6 8] [8 9] [10 8]) (conj [6 9] [10 9]))))
    (is (= (set (get-nodes (remove-propagate graph 4))) (-> graph-nodes (disj 4))))
    (is (= (set (get-links (remove-propagate graph 4))) (-> graph-links (disj [2 4] [4 7] [4 1]) (conj [2 1])))))
  (testing "-trans-closure"
    (is (= (set (get-nodes (-trans-closure graph))) graph-nodes)))
    (is (= (set (get-links (-trans-closure graph))) (-> graph-links
                                                      (into (map #(vector 1 %) [4 5 7]))
                                                      (into (map #(vector 2 %) [1 3 5]))
                                                      (into (map #(vector 4 %) [1 2 3 5]))
                                                      (conj [3 7] [6 9] [8 6] [8 10] [9 8] [9 10] [10 9] [10 6]))))
  (testing "trans-closure"
    (is (= (set (get-nodes (trans-closure graph))) graph-nodes)))
    (is (= (set (get-links (trans-closure graph))) (-> graph-links
                                                     (into (map #(vector 1 %) [4 5 7]))
                                                     (into (map #(vector 2 %) [1 3 5]))
                                                     (into (map #(vector 4 %) [1 2 3 5]))
                                                     (conj [3 7] [6 9] [8 6] [8 10] [9 8] [9 10] [10 9] [10 6]))))
  (testing "trans-reduction"
    (is (= (set (get-nodes (trans-reduction (break-loops graph)))) graph-nodes))
    (is (= (set (get-links (trans-reduction (break-loops graph)))) (-> graph-links (disj [4 1] [9 6] [2 7] [6 8])))))
  (testing "subgraph"
    (is (= (set (get-nodes (subgraph graph [2 3 4 5 6 7 8 10]))) (disj graph-nodes 1 9)))
    (is (= (set (get-links (subgraph graph [2 3 4 5 6 7 8 10]))) (disj graph-links [1 2] [4 1] [1 3] [8 9] [9 6]))))
 (testing "subgraph-reduced"
   (is (= (set (get-nodes (subgraph-reduced (break-loops graph) [2 3 4 5 6 7 8 10]))) (disj graph-nodes 1 9)))
   (is (= (set (get-links (subgraph-reduced (break-loops graph) [2 3 4 5 6 7 8 10]))) (disj graph-links [1 2] [4 1] [1 3] [8 9] [9 6] #_"transitive links" [2 7] [6 8]))))
 (testing "merge-graphs"
   (is (= (set (get-nodes (merge-graphs graph graph))) graph-nodes))
   (is (= (set (get-links (merge-graphs graph graph))) graph-links))
   (is (= (set (get-nodes (merge-graphs graph (-> (digraph) (add-links-safe [[1 11] [5 6]]))))) (conj graph-nodes 11)))
   (is (= (set (get-links (merge-graphs graph (-> (digraph) (add-links-safe [[1 11] [5 6]]))))) (conj graph-links [1 11] [5 6]))))
 (testing "distances-from"
   (is (= (distances-from graph 4) {4 0 1 1 7 1 2 2 3 2 5 3}))
   (is (= (distances-from graph 4 :direction :backward) {4 0 2 1 1 2})))
 (testing "dag-distance"
   (is (= (dag-distance graph 4 5) 3))
   (is (= (dag-distance graph 1 7) 2))
   (is (= (dag-distance graph 3 7) 2))
   (is (= (dag-distance graph 1 8) nil)))
 (testing "weakly-connected"
   (is (= (set (map set (weakly-connected graph))) #{#{1 2 3 4 5 7} #{6 8 9 10}})))
 (testing "topological"
   (let [order (topological (break-loops graph))]
     (is (> (.indexOf order 7) (.indexOf order 2)))
     (is (> (.indexOf order 7) (.indexOf order 4)))
     (is (> (.indexOf order 7) (.indexOf order 5)))
     (is (> (.indexOf order 4) (.indexOf order 2)))
     (is (> (.indexOf order 2) (.indexOf order 1)))
     (is (> (.indexOf order 3) (.indexOf order 1)))
     (is (> (.indexOf order 5) (.indexOf order 3)))
     (is (> (.indexOf order 9) (.indexOf order 8)))
     (is (> (.indexOf order 8) (.indexOf order 6)))
     (is (> (.indexOf order 8) (.indexOf order 10)))
     (is (> (.indexOf order 10) (.indexOf order 6)))))
 (testing "dag-height"
     (is (= (dag-height (break-loops graph)) 3))))
