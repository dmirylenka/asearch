(ns graphs.core
  (:require [clojure [string :as string]
                     [set :as set]]
            [clojure.core [reducers :as r]]))

(defprotocol IDigraphInternal
  (get-nodes [this])
  (contains [this node])
  (out-links [this node])
  (in-links [this node])
  (-add-node [this node])
  (-remove-node [this node])
  (-add-link [this from to])
  (-remove-link [this from to])
  (-subgraph [this nodes])
  (merge-reversed [this]))

(defn- filter-links [link-map node-set]
  (->> link-map
    (r/filter (fn [[k v]] (contains? node-set k)))
    (r/map (fn [[k v]] [k (set/intersection node-set v)]))
    (into {})))

(defrecord Digraph [nodes in-map out-map] ;assumes nodes is a set
  IDigraphInternal
    (get-nodes [this] nodes)
    (contains [this node] (contains? nodes node))
    (out-links [this node]
      (out-map node))
    (in-links [this node]
      (in-map node))
    (-add-node [this new-node]
      (Digraph. (conj nodes new-node)
                (assoc in-map new-node #{})
                (assoc out-map new-node #{})))
    (-remove-node [this node]
      (Digraph. (disj nodes node)
                (dissoc in-map node)
                (dissoc out-map node))) 
    (-add-link [this from to]
      (Digraph. nodes
                (update-in in-map [to] conj from)
                (update-in out-map [from] conj to)))
    (-remove-link [this from to]
      (Digraph. nodes
                (update-in in-map [to] disj from)
                (update-in out-map [from] disj to)))
    (merge-reversed [this]
      (let [link-map (merge-with into in-map out-map)]
        (Digraph. nodes link-map link-map)))
    (-subgraph [this nodes]
      (let [node-set (set nodes)]
        (Digraph. node-set (filter-links in-map node-set)
                  (filter-links out-map node-set))))
  Object
    (toString [this]
      (str "nodes " (.toString nodes) ", out-map " (.toString out-map))))

(defn digraph [] (Digraph. #{} {} {}))

(defn get-links [graph]
  (for [node (get-nodes graph)
        out-node (out-links graph node)]
    [node out-node]))

(defn add-node [graph node]
  (when (contains graph node)
    (throw (Exception. (str "Adding node failed: already exists: " (pr-str node)))))
  (-add-node graph node))

(defn add-nodes [graph nodes] ; add efficient addition of nodes when possible
  (reduce add-node graph nodes))

(defn remove-node-safe [graph node]
  (let [outs (out-links graph node)
        ins (in-links graph node)
        links-to-remove (concat (map #(vector node %) outs)
                                (map #(vector % node) ins))]
    (-> graph
      (#(reduce (partial apply -remove-link) % links-to-remove))
      (-remove-node node))))

;TODO: add efficient removal of nodes when possible
(defn remove-nodes-safe [graph nodes]
  (reduce remove-node-safe graph nodes))

(defn contains-link [graph from to]
  (contains? (out-links graph from) to))

(defn add-link [graph from to]
  (when-not (and (contains graph from) (contains graph to) (not (contains-link graph from to)))
    (throw (Exception. "Adding link failed.")))
  (-add-link graph from to))

(defn add-links [graph from-to-pairs]
  (reduce (partial apply add-link) graph from-to-pairs))

(defn remove-link [graph from to]
  (when-not (contains-link graph from to)
    (throw (Exception. "Removing link failed.")))
  (-remove-link graph from to))

(defn remove-links [graph from-to-pairs]
  (reduce (partial apply remove-link) graph from-to-pairs))

(defn add-node-safe [graph node]
  (if-not (contains graph node)
    (-add-node graph node)
    graph))

(defn add-nodes-safe [graph nodes]
  (reduce add-node-safe graph nodes))

(defn add-link-safe [graph from to]
  (if-not (contains-link graph from to)
    (-> graph
      (add-node-safe from)
      (add-node-safe to)
      (-add-link from to))
    graph))

(defn add-links-safe [graph from-to-pairs]
  (reduce (partial apply add-link-safe) graph from-to-pairs))

(defprotocol IGraphNode ; so far used only for drawing
  (node-id [this])
  (node-title [this]))

(extend-protocol IGraphNode
  String
    (node-id [this] this)
    (node-title [this] this))

(defrecord GraphNode [obj id-fn title-fn]
  IGraphNode 
    (node-id [this] (id-fn this))
    (node-title [this] (title-fn this)))

(defn wrap-node [obj id-fn title-fn]
  (GraphNode. obj id-fn title-fn))

(defn graph2dot
  ([graph & more]
    (let [opt (apply hash-map more)
          nodes (get-nodes graph)
          id-fn (or (:id-fn opt) node-id)
          name-fn (or (:name-fn opt) node-title)
          font-fn (or (:font-fn opt) (constantly 10))
          relations (for [node nodes adj (out-links graph node)]
                      (map id-fn [node adj]))]
      (str "digraph G { rankdir=LR; nodesep=0.1;"
           (->> relations
             (map #(str "\"" (first %) "\"" " -> "  "\""(second %) "\" [arrowsize=0.25, color=grey]"))
             (string/join "; "))
           (if (seq relations) "; " "")
           (->> nodes
             (map #(str "\"" (id-fn %) "\"" " ["
                        "label=" "\"" (name-fn %) "\""
                        ", shape=box"
                        ", fontsize=" (font-fn %) ", margin=\"0.1,0.1\""
                        ", fontname=Arial"
                        ", style=\"rounded,filled\", color=\"#eeeeff\""
                        "]"))
             (string/join "; "))
           "}"))))

(defn search [graph & more]
  (let [opt (apply hash-map more)
        node-order (vec (or (:node-order opt) (get-nodes graph)))
        ignore (fn [& args] (first args))
        descend (or (:descend opt) ignore)
        leave (or (:leave opt) ignore)
        restart (or (:restart opt) ignore)
        discover (or (:discover opt) ignore)
        dejavu (or (:dejavu opt) ignore)
        mode (or (:mode opt) :dfs)
        direction (or (:direction opt) :forward)
        adj (case direction
              :forward out-links
              :backward in-links)
        iter (fn iter [{:keys [seen-from visited explored schedule nodes-left client-env]
                        :as env}]
               (if (empty? schedule)
                 (let [nodes-left (drop-while explored nodes-left)
                       node (first nodes-left)]
                   (if (empty? nodes-left)
                     env
                     (recur (-> env
                              (update-in [:seen-from] assoc node nil)
                              (update-in [:schedule] conj node)
                              (assoc :nodes-left (rest nodes-left))
                              (update-in [:client-env] restart node)
                              (update-in [:client-env] discover node)))))
                 (let [node (first schedule)
                       adjacent (adj graph node)
                       seen (remove #(= node (get seen-from % node)) adjacent) 
                       unseen (remove #(contains? seen-from %) adjacent)
                       env (if (visited node)
                             env
                             (-> env
                               (update-in [:client-env] (fn [cl-env] (reduce #(dejavu %1 node %2) cl-env seen)))
                               (update-in [:visited] conj node)))]
                   (if-not (empty? unseen)
                     (case mode
                       :dfs (recur (-> env
                                     (update-in [:seen-from] assoc (first unseen) node)
                                     (update-in [:schedule] conj (first unseen))
                                     (update-in [:client-env] descend node (first unseen))
                                     (update-in [:client-env] discover (first unseen))))
                       :bfs (recur (-> env
                                     (update-in [:seen-from] into (map #(vector % node) unseen))
                                     (update-in [:schedule] #(into unseen %))
                                     (update-in [:client-env] (fn [cl-env] (reduce #(descend %1 node %2) cl-env unseen)))
                                     (update-in [:client-env] #(reduce discover % unseen)))))
                     (recur (-> env
                              (update-in [:explored] conj node)
                              (update-in [:schedule] rest)
                              (update-in [:client-env] leave node)))))))
        result (iter {:seen-from {} :visited #{} :explored #{} :schedule '()
                      :nodes-left node-order :client-env (:env opt)})]
    (:client-env result)))

(defn break-loops [graph]
  (let [all-nodes (get-nodes graph)
        no-parents? #(empty? (in-links graph %))
        root-nodes (filter no-parents? all-nodes)
        smart-order (concat root-nodes (remove (set root-nodes) all-nodes))
        start-env {:stack [] :set #{} :back-links []}
        unwind-until (fn [stack node]
                       (if (= node (peek stack))
                         stack
                         (recur (pop stack) node)))
        descend (fn [env node next-node]
                  (update-in env [:stack] #(-> % (unwind-until node) (conj next-node))))
        restart (fn [env node] (assoc env :stack [node]))
        dejavu (fn [env node next-node]
                 (if (some #{next-node} (:stack env))
                   (update-in env [:back-links] conj [node next-node])
                   env))
        search-run (search graph :mode :dfs :env start-env :node-order smart-order
                           :descend descend :dejavu dejavu :restart restart)
        back-links (:back-links search-run)]
    (remove-links graph back-links)))

#_(defn reachable [graph nodes & more]
  (let [opt (apply hash-map more)]
    (search graph :node-order nodes :mode :bfs :direction (:direction opt)
            :env [] :discover conj)))

(defn reachable [graph nodes & more]
  (let [opt (apply hash-map more)
        dir (or (:direction opt) :forward)
        adj (case dir
              :forward (partial out-links graph)
              :backward (partial in-links graph))
        iter (fn iter [all-nodes new-nodes]
               (if (empty? new-nodes)
                 all-nodes
                 (let [next-nodes (set (remove all-nodes (mapcat adj new-nodes)))]
                   (recur (into all-nodes next-nodes)
                          next-nodes))))]
    (iter (set nodes) (set nodes))))

(defn merge-nodes [graph node-keeped node-removed]
  (let [links-to-add (concat
                       (for [out-node (out-links graph node-removed)
                             :when (not= out-node node-keeped)]
                         [node-keeped out-node])
                       (for [in-node (in-links graph node-removed)
                             :when (not= in-node node-keeped)]
                         [in-node node-keeped]))]
    (-> graph
      (remove-node-safe node-removed)
      (add-links-safe links-to-add))))

(defn replace-node [graph old-node new-node]
  (when-not (and (contains graph old-node)
                 #_(not (contains graph new-node)))
    (throw (Exception. (pr-str "Replacing node failed: " old-node " -> " new-node))))
  (let [outs (out-links graph old-node)
        ins (in-links graph old-node)
        links-to-add (concat (map #(vector new-node %) outs)
                             (map #(vector % new-node) ins))]
    (-> graph
      (remove-node-safe old-node)
      (add-node-safe new-node) 
      (add-links-safe links-to-add))))

(defn remove-propagate [graph node]
  (let [outs (out-links graph node)
        ins (in-links graph node)
        new-links (for [in ins
                        out outs]
                    [in out])]
    (-> graph
      (remove-node-safe node)
      (add-links-safe new-links))))

;(defn subgraph [graph nodes & more]
;  (let [nodes-to-remove (remove (set nodes) (get-nodes graph))
;        remove-fn (if (some #{:propagate} more)
;                    remove-propagate
;                    remove-node)]
;    (reduce remove-fn graph nodes-to-remove)))

(defn -trans-closure [graph]
  (let [nodes (get-nodes graph)
        links (for [node nodes
                    out-node (reachable graph [node])
                    :when (not= node out-node)]
                [node out-node])]
    (add-links-safe graph links)))

(defn cache-closure [graph]
  (assoc graph :closure (-trans-closure graph)))

(defn trans-closure [graph]
  (or (:closure graph) (-trans-closure graph)))

(defn trans-reduction [graph]
  (let [closure (trans-closure graph)
        exists-long-path
          (fn [node1 node2]
            (let [children (out-links graph node1)]
              (some #(contains-link closure % node2) children)))
        links-to-remove
          (for [node (get-nodes graph)
                out-node (out-links graph node)
                :when (exists-long-path node out-node)]
            [node out-node])]
    (remove-links graph links-to-remove)))

(defn subgraph-reduced [graph nodes]
  (-> graph trans-closure (-subgraph nodes) trans-reduction))

;(defn inc-subgraph-reduced [graph subgr node]
;  (let [closure (trans-closure graph)
;        new-links (concat (map #(vector node %) (out-links graph node))
;                          (map #(vector % node) (in-links graph node)))]
;  (-> graph trans-closure (-subgraph nodes) trans-reduction)))

(defn subgraph [graph nodes]
  (-subgraph graph nodes))

(defn merge-graphs [graph1 graph2]
   (-> graph1 
     (add-nodes-safe (get-nodes graph2))
     (add-links-safe (get-links graph2))))

(defn distances-from [graph node & more]
  (let [opt (apply hash-map more)
        restart (fn [distances start-node] (assoc distances start-node 0)) 
        descend (fn [distances node-from node-to]
                  (assoc distances node-to (inc (distances node-from))))] 
    (search graph :node-order [node] :mode :bfs :direction (:direction opt)
            :env {} :restart restart :descend descend)))

;inefficient implementation: computes paths up to the roots (not only to the closest common ancestor)
(defn dag-distance
  "Computes the length of the shortest path between two nodes that goes through a common ancestor." 
  [graph node1 node2]
  (let [dist1 (distances-from graph node1 :direction :backward)
        dist2 (distances-from graph node2 :direction :backward)
        common-parents (filter (set (keys dist1)) (keys dist2))
        score-fn #(+ (dist1 %) (dist2 %))
        closest-parent (when-not (empty? common-parents)
                         (apply min-key score-fn common-parents))]
    (when closest-parent (score-fn closest-parent))))

(defn weakly-connected
  "Weakly-connected components of the graph."
  [graph]
  (let [descend (fn [wccs from to]
                  (conj (pop wccs) (conj (peek wccs) to)))
        restart (fn [wccs node]
                  (conj wccs (list node)))]
    (search (merge-reversed graph) :env '() :descend descend :restart restart)))

(defn topological
  "Topological order for the graph"
  [graph & {:keys [direction]}]
  (search graph :env '() :leave conj :direction direction))

(defn dag-height
  "Computes the 'height' of a DAG, that is the length of the longest path in it."
  [graph]
  (let [nodes (topological graph)
        lengths (zipmap nodes (repeat 0)) 
        compute-length (fn [lengths node]
                         (let [parents (in-links graph node)
                               inc-length #(inc (apply max (map lengths %)))]
                           (cond-> lengths (seq parents)
                                   (assoc node (inc-length parents)))))
        lengths (reduce compute-length lengths nodes)]
    (apply max (vals lengths))))

; Implementation of removing the node that safely removes the links
; (remove-node [this node] ; check if it has links to anything else?
;   (let [nodes* (disj nodes node)
;         out-nodes (out-map node)
;         out-map* (dissoc out-map node)
;         in-map* (loop [nodes-to-clean out-nodes
;                        in-map* (transient in-map)]
;                   (if (seq nodes-to-clean)
;                     (let [first-node (first nodes-to-clean)
;                           rest-nodes (rest nodes-to-clean)
;                           out-nodes-ins (in-map first-node)]
;                       (recur rest-nodes 
;                              (assoc! in-map* first-node
;                                      (into (empty out-nodes-ins)
;                                            (remove #{node} out-nodes-ins)))))
;                     (persistent! in-map*)))]
;     (Digraph. nodes* in-map* out-map*)))
