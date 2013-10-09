(ns topic-maps.features
  (:use topic-maps.core)
  (:require [utils.core :as u]
            [graphs.core :as g]
            [clojure [set :as set]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn freq
  "Number of documents directy associated with the topic."
  [topic-map topic]
  (count (proper-docs topic-map topic)))

(defn subtopics
  [topic-map topic & {:keys [proper direct] :or {proper true direct true}}]
  (let [{:keys [topic-graph]} topic-map
        children (g/out-links topic-graph topic)
        topic-when-contains (if (g/contains topic-graph topic) [topic] [])]
    (if direct
      (if proper
        children
        (into children topic-when-contains))
      (g/reachable topic-graph (if proper children topic-when-contains)))))

(defn covered-docs
  "Documents covered by given topics, also indirectly."
  [topic-map topics]
  (let [{:keys [topic-graph topic-docs]} topic-map]
    (->> (g/reachable topic-graph topics)
      (mapcat (partial g/out-links topic-docs))
      distinct)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Features of the topic-maps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defn paper-coverage
;  "Percent of documents covered by given topics."
;  [topic-map submap]
;  (/ (n-docs submap)
;     (n-docs topic-map)))

;rewritten
(defn paper-coverage
  "Percent of documents covered by given topics."
  ([topic-map] {:value 0 :docs #{} :ndocs-total (n-docs topic-map)})
  ([topic-map submap feature new-topic]
   (let [{:keys [docs ndocs-total]} feature
         new-docs (covered-docs topic-map [new-topic])
         docs (into docs new-docs)
         ndocs (count docs)]
     (assoc feature
       :docs docs
       :value (/ ndocs ndocs-total)))))

;(defn direct-doc-coverage
;  "Percent of documents covered by given topics."
;  [topic-map submap]
;  (/ (->> submap get-topics (mapcat #(proper-docs topic-map %)) set count)
;     (n-docs topic-map)))

;rewritten
(defn direct-doc-coverage
  "Percent of documents covered by given topics."
  ([topic-map] {:value 0 :docs #{} :ndocs-total (n-docs topic-map)})
  ([topic-map submap feature new-topic]
   (let [{:keys [docs ndocs-total]} feature
         docs (into docs (proper-docs topic-map new-topic))]
     (assoc feature
            :docs docs
            :value (/ (count docs) ndocs-total)))))

(defn distant-paper-coverage
  "Distance-weighted paper coverage."
  [topic-map submap]
  (let [ndocs (n-docs topic-map) 
        topics (get-topics submap)
        iter (fn iter [reached-docs reached-topics last-topics]
               (if (empty? last-topics)
                 reached-docs
                 (let [next-docs (->> last-topics
                                  (mapcat (partial proper-docs topic-map))
                                  (remove (set (flatten reached-docs)))
                                  distinct)
                       next-topics (->> last-topics
                                    (mapcat (partial child-topics topic-map))
                                    (remove reached-topics)
                                    distinct)]
                   (recur (conj reached-docs next-docs)
                          (into reached-topics next-topics)
                          next-topics))))]
    (map #(/ (count %) ndocs) (iter [] (set topics) topics))))

(defn topic-coverage
  [topic-map submap]
  (/ (n-topics submap) (n-topics topic-map)))

;(defn avg-topic-freq
;  "Percent of documents covered by given topics."
;  [topic-map submap]
;  (/ (u/avg (map (partial freq topic-map) (get-topics submap)))
;     (n-docs topic-map)))

;rewritten
(defn avg-topic-freq
  "Percent of documents covered by given topics."
  ([topic-map] {:value 0 :freqs [] :ndocs-total (n-docs topic-map)})
  ([topic-map submap feature new-topic]
   (let [{:keys [freqs ndocs-total]} feature
         freqs (conj freqs (freq topic-map new-topic))]
     (assoc feature
            :freqs freqs
            :value (/ (u/avg freqs) ndocs-total)))))

;(defn min-topic-freq
;  "Percent of documents covered by given topics."
;  [topic-map submap]
;  (/ (apply min (map (partial freq topic-map) (get-topics submap)))
;     (n-docs topic-map)))

;rewritten
(defn min-topic-freq
  "Percent of documents covered by given topics."
  ([topic-map] {:value 0 :freqs [] :ndocs-total (n-docs topic-map)})
  ([topic-map submap feature new-topic]
   (let [{:keys [freqs ndocs-total]} feature
         freqs (conj freqs (freq topic-map new-topic))]
     (assoc feature
            :freqs freqs
            :value (/ (apply min freqs) ndocs-total)))))

;(defn avg-cum-freq
;  [topic-map submap]
;  (/ (u/avg (map #(count (covered-docs submap [%])) (get-topics submap)))
;     (n-docs topic-map)))

;rewritten
(defn avg-cum-freq
  ([topic-map] {:value 0 :freqs [] :ndocs-total (n-docs topic-map)})
  ([topic-map submap feature new-topic]
   (let [{:keys [freqs ndocs-total]} feature
         freqs (conj freqs (count (covered-docs submap [new-topic])))]
     (assoc feature
            :freqs freqs
            :value (/ (u/avg freqs) ndocs-total)))))

;(defn min-cum-freq
;  [topic-map submap]
;  (/ (apply min (map #(count (covered-docs submap [%])) (get-topics submap)))
;     (n-docs topic-map)))

(defn min-cum-freq
  ([topic-map] {:value 0 :freqs [] :ndocs-total (n-docs topic-map)})
  ([topic-map submap feature new-topic]
   (let [{:keys [freqs ndocs-total]} feature
         freqs (conj freqs (count (covered-docs submap [new-topic])))]
     (assoc feature
            :freqs freqs
            :value (/ (apply min freqs) ndocs-total)))))

(defn distant-topic-coverage
  "Distance-weighted topic-coverage."
  [topic-map submap & more]
  (let [opt (apply hash-map more)
        ntopics (n-topics topic-map)
        total-sum-freqs (apply + (map (partial freq topic-map) (get-topics topic-map)))
        count-mode (or (:count opt) :topics)
        count-fn (case count-mode
                   :topics #(/ (count %) ntopics)
                   :freqs #(/ (apply + (map (partial freq topic-map) %)) total-sum-freqs))
        iter (fn iter
               ([topics] (iter [topics] topics))
               ([reached-topics last-topics]
                (let [next-topics (->> last-topics
                                    (mapcat (partial child-topics topic-map))
                                    (remove (set (apply concat reached-topics)))
                                    distinct)]
                  (if (empty? next-topics)
                    reached-topics
                    (recur (conj reached-topics next-topics)
                           next-topics)))))]
    (map count-fn (iter (get-topics submap)))))

(defn pad-to-size [n feature]
  (fn [topic-map submap]
    (let [result (feature topic-map submap)]
      (vec (take n (concat result (repeat 0)))))))

(defn vector-feature [n feature]
  (let [padded-ftr (pad-to-size n feature)
        result (atom nil) 
        ftr #(fn [topic-map submap]
               (when-not @result
                 (swap! result (constantly (padded-ftr topic-map submap))))
               (get @result %))]
    (map ftr (range n))))

;(defn partition-coef
;  "Partition coefficient of the topic map viewed as fuzzy clustering.
;   Equals the average squared membership level across all elements and clusters."
;  [topic-map submap]
;  (let [{:keys [topic-graph topic-docs]} submap
;        ntopics (fn ntopics [doc]
;                  (->> doc
;                    (g/in-links topic-docs)
;                    (#(g/reachable topic-graph % :direction :backward))
;                    count))]
;    (if (< (n-docs submap) 2) 1 
;      (->> (get-docs submap) (map ntopics) (map #(/ 1 %)) u/avg))))

;rewritten
(defn partition-coef
  "Partition coefficient of the topic map viewed as fuzzy clustering.
   Equals the average squared membership level across all elements and clusters."
  ([topic-map] {:value 1 :doc-topic-counts {}})
  ([topic-map submap feature new-topic]
    (let [{:keys [doc-topic-counts]} feature
        ; {:keys [topic-graph topic-docs]} submap
          docs (covered-docs topic-map [new-topic])
          doc-topic-counts (merge-with + doc-topic-counts
                             (u/val-map (constantly 1) docs))]
      (assoc feature
             :doc-topic-counts doc-topic-counts
             :value (if (< (n-docs submap) 2) 1 
                      (->> doc-topic-counts vals (map #(/ 1 %)) u/avg))))))

;(defn avg-pairwise-dist
;  "Avegage pairwise distance between the topics in the map.
;   Distance between two nodes is the length of the shortest undirected path going through a common ancestor."
;  [topic-map submap]
;  (if (= 1 (n-topics submap)) 0
;    (let [{:keys [topic-graph topic-docs doc-map]} topic-map
;          topics (get-topics submap)
;          topic-pairs (for [topic1 topics topic2 topics
;                            :when (< 0 (.compareTo (str topic1) (str topic2)))]
;                        [topic1 topic2])
;          distances (map (partial apply g/dag-distance topic-graph) topic-pairs)
;          map-01 (fn [dist] (if dist (/ dist (inc dist)) 1))]
;      (u/avg (map map-01 distances)))))

(defn map-01 [x]
  (if x (/ x (+ 1 x)) 1))

(defn always-one
  "Feature that is always equal to one."
  ([topic-map] {:value 1})
  ([topic-map submap old-value new-topic] {:value 1}))

;rewritten
(defn avg-pairwise-dist
  "Avegage pairwise distance between the topics in the map.
   Distance between two nodes is the length of the shortest undirected path going through a common ancestor."
  ([topic-map] {:value 0 :distances []})
  ([topic-map submap old-value new-topic]
   (let [{:keys [distances]} old-value
         {:keys [topic-graph]} topic-map
         topics (get-topics submap)
         new-dist (mapv #(g/dag-distance topic-graph new-topic %) topics)
         all-dist (into distances new-dist)]
     {:distances all-dist
      :value (if (empty? all-dist) 0
               (u/avg (map map-01 all-dist)))})))

(defn unevenness
  "Measures how uneven in size are sibling topics and averages the result across all sibling topic sets.
   For a set of sibling topic unevennes equals to the sum of squared ratios of the topic size to the sum of topic sizes.  
   Unevennes is corrected for the number of sibling topics, in order to range from 0 to 1 (rather then from 1/(# topics) to 1).
   The topic size is measured as the number of transitively covered documents."
  [topic-map submap]
  (let [{:keys [topic-graph]} submap
        covered-docs (memoize #(covered-docs submap [%]))
        norm-pc (fn norm-pc [set-sizes]
                  (let [total (apply + set-sizes)
                        probs (map #(/ % total) set-sizes)
                        pc (apply + (map #(* % %) probs))
                        min-pc (/ 1 (count set-sizes))]
                    (if (= pc min-pc) 0 (/ (- pc min-pc) (- 1 min-pc)))))
        sibling-sets (for [topic (get-topics submap)
                           :let [children (child-topics submap topic)]
                           :when (seq children)]
                       children)
        siblings->sizes (partial map (comp count covered-docs))
        siblings->pc (comp norm-pc siblings->sizes)]
    (if (empty? sibling-sets) 0
      (u/avg (map siblings->pc sibling-sets)))))

(defn subtopic-coverage 
  "Measures the percent of displayed subtopics averaged across all displayed topics that have at least one subtopic displayed.
   With ':direct true' (which is default) takes into account only direct subtopics, otherwise includes also transitive ones."
  [topic-map submap & {:keys [direct] :or {direct true}}]
  (let [{:keys [topic-graph]} topic-map
        topic-set (set (get-topics submap))
        coverage (for [topic topic-set 
                       :let [all-children (subtopics topic-map topic :direct direct)
                             displayed-children (filter topic-set all-children)]
                       :when (seq displayed-children)]
                   (/ (count displayed-children) (count all-children)))]
    (if (empty? coverage) 1
      (u/avg coverage))))

(defn v-structures
  "Measures the number of V structures (with :count :vstruct) or the number of nodes (with :count :nodes) in the topic map,
   divided by the maximum possible value of this number for given number of topics.
   When ':reversed true' counts the V-structures in the reversed graph, that is /\\ structures in the orginal one."
  [topic-map submap  & more]
  (let [{:keys [topic-graph]} submap
        opt (apply hash-map more)
        cnt (or (:count opt) :vstruct)
        reversed (or (:reversed opt) false)
        npar->nvstruct #(/ (* % (dec %)) 2)
        npar->1 (constantly 1) 
        count-fn (case cnt
                   :vstruct npar->nvstruct
                   :nodes npar->1)
        topics (g/get-nodes topic-graph)
        ntopics (count topics)
        max-vstruct (npar->nvstruct (dec ntopics))
        max-nodes (- ntopics 2)
        norm-const (case cnt
                     :vstruct max-vstruct
                     :nodes max-nodes)
        adj (if-not reversed g/in-links g/out-links)
        nadjacent #(count (adj topic-graph %))]
    (if (< ntopics 3) 0 
      (->> topics
        (map nadjacent)
        (filter #(> % 1))
        (map count-fn)
        (apply +)
        (#(/ % norm-const))))))

;(defn n-connected
;  "Computes the number of connected components in a topic submap."
;  ([topic-map submap]
;   (/ (count (g/weakly-connected (:topic-graph submap)))
;      (n-topics submap))))

;rewritten
(defn n-connected
  "Computes the number of connected components in a topic submap."
  ([topic-map]
   (let [components (g/weakly-connected (:topic-graph topic-map))
         comp-id-map (into {} (for [[component id] (map vector components (range))
                                    vertex component]
                                [vertex id]))]
     {:value 0 :comp-id-map comp-id-map :comp-ids #{}}))
  ([topic-map submap feature  new-topic]
   (let [{:keys [comp-id-map value comp-ids]} feature
         comp-id (comp-id-map new-topic)
         comp-ids (conj comp-ids comp-id)
         ntopics (inc (n-topics submap))]
     (assoc feature
            :comp-id-map comp-id-map
            :comp-ids comp-ids
            :value (/ (count comp-ids) ntopics)))))

;; TODO normalize ?
;(defn n-links
;  "Computes the number of links in the topic sub-map."
;  [topic-map submap]
;    (->> submap get-topics (mapcat (partial child-topics submap)) count))
;(defn n-links
;  "Computes the number of links in the topic sub-map."
;  [topic-map submap]
;    (->> submap get-topics (mapcat (partial child-topics submap)) count))

; TODO normalize ?
(defn avg-n-adj
  "Average number of parents (with :mode :parent) or children (with :mode :child) for a node that has parents or children respectively.
   This measure should be computed on the postprocessed topic submap (without shortcut links, etc.)."
  [topic-map submap & {:keys [mode] :or {mode :parent}}]
  (let [adj (case mode
              :parent #(parent-topics submap %)
              :child #(child-topics submap %))
        nadj (comp count adj)
        safe-avg #(if (empty? %) 1 (u/avg %))]
    (dec (->> submap get-topics (map nadj) (remove zero?) safe-avg))))

(defn max-n-adj
  "Average number of parents (with :mode :parent) or children (with :mode :child) for a node that has parents or children respectively.
   This measure should be computed on the postprocessed topic submap (without shortcut links, etc.)."
  [topic-map submap & {:keys [mode] :or {mode :parent}}]
  (let [adj (case mode
              :parent #(parent-topics submap %)
              :child #(child-topics submap %))
        nadj (comp count adj)
        safe-max #(if (empty? %) 1 (apply max %))]
    (dec (->> submap get-topics (map nadj) (remove zero?) safe-max))))

;(defn main-subtopics
;  "Percent of nodes covered by the subhierarchy of the main topic."
;  [topic-map {:as submap :keys [main-topic]}]
;  {:pre [main-topic]}
;  (/ (count (subtopics submap main-topic :proper false :direct false))
;     (n-topics submap)))

;rewritten
(defn main-subtopics
  "Percent of nodes covered by the subhierarchy of the main topic."
  ([topic-map]
   (let [{:keys [main-topic]} topic-map]
     {:value 0
      :nsubt 0
      :subt (set (subtopics topic-map main-topic :proper false :direct false))}))
  ([topic-map submap feature new-topic]
   {:pre [main-topic]}
   (let [{:keys [subt nsubt]} feature
         nsubt (cond-> nsubt (subt new-topic) inc)]
   (assoc feature
          :nsubt nsubt 
          :value (/ nsubt (inc (n-topics submap)))))))

(defn height
  "Computes the length of the longest path in the topic map."
  [topic-map submap]
  (let [ntopics (n-topics submap)]
    (if (< ntopics 2) 0
      (/ (g/dag-height (:topic-graph submap))
         (dec ntopics)))))

(defn n-small
  "The number of topics of size 1."
  [topic-map submap]
  (/ (count (filter #(< (count (covered-docs submap [%])) 2) (get-topics submap)))
     (n-topics submap)))

(defn avg-pc-overlap
  "Average overlap between parent and child topic."
  [topic-map submap]
  (let [overlaps (for [parent (get-topics submap)
                       child (child-topics submap parent)]
                   (/ (count (covered-docs submap [child]))
                      (count (covered-docs submap [parent]))))]
    (if (empty? overlaps) 0
      (u/avg overlaps))))

(defn max-pc-overlap
  "Maximum overlap between parent and child topic."
  [topic-map submap]
  (let [overlaps (for [parent (get-topics submap)
                       child (child-topics submap parent)]
                   (/ (count (covered-docs submap [child]))
                      (count (covered-docs submap [parent]))))]
    (if (empty? overlaps) 0
      (apply max overlaps))))

;(defn avg-overlap
;  "Average overlap between any two topics."
;  [topic-map submap]
;  (let [overlaps (for [parent (get-topics submap)
;                       child (get-topics submap)
;                       :let [child-docs (set (covered-docs submap [child]))
;                             parent-docs (set (covered-docs submap [parent]))]
;                       :when (and (seq child-docs) (seq parent-docs))]
;                   (/ (count (set/intersection child-docs))
;                      (count (set/union parent-docs))))]
;    (if (empty? overlaps) 0
;      (u/avg overlaps))))

;rewritten
(defn avg-overlap
  "Average overlap between any two topics."
  ([topic-map] {:value 0 :overlaps [] :doc-sets []})
  ([topic-map submap feature new-topic]
   (let [{:keys [overlaps doc-sets]} feature
         new-topic-docs (set (covered-docs topic-map [new-topic]))
         new-overlaps (for [docs doc-sets
                            :when (and (seq docs) (seq new-topic-docs))]
                        (/ (count (set/intersection docs new-topic-docs))
                           (count (set/union docs new-topic-docs))))
         overlaps (into overlaps new-overlaps)
         doc-sets (conj doc-sets new-topic-docs)]
     (assoc feature
            :overlaps overlaps
            :doc-sets doc-sets
            :value (if (empty? overlaps) 0 (u/avg overlaps))))))

;(defn max-overlap
;  "Maximum overlap between any two topics." 
;  [topic-map submap]
;  (let [overlaps (for [parent (get-topics submap)
;                       child (get-topics submap)
;                       :let [child-docs (set (covered-docs submap [child]))
;                             parent-docs (set (covered-docs submap [parent]))]
;                       :when (and (seq child-docs) (seq parent-docs))]
;                   (/ (count (set/intersection child-docs))
;                      (count (set/union parent-docs))))]
;    (if (empty? overlaps) 0
;      (apply max overlaps))))
  
;rewritten
(defn max-overlap
  "Maximum overlap between any two topics."
  ([topic-map] {:value 0 :doc-sets []})
  ([topic-map submap feature new-topic]
   (let [{:keys [value doc-sets]} feature
         new-topic-docs (set (covered-docs topic-map [new-topic]))
         new-overlaps (for [docs doc-sets
                            :when (and (seq docs) (seq new-topic-docs))]
                        (/ (count (set/intersection docs new-topic-docs))
                           (count (set/union docs new-topic-docs))))
         doc-sets (conj doc-sets new-topic-docs)]
     (assoc feature
            :doc-sets doc-sets
            :value (apply max (conj new-overlaps value))))))
