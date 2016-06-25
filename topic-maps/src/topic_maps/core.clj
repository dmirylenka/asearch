(ns topic-maps.core
;; (:use [graphs.core :only [IGraphNode]])
  (:require [wminer.core :as wminer]
            [mlinking.core :as mlinking]
            [wiki-api.core :as wapi]
            [utils [core :as u]
                   [text :as t]]
            [graphs.core :as g]
            [dot-api.core :as dot]
            [clojure [set :as set]
            [string :as string]])
  (:import (graphs.core Digraph)))

;;;;;;;;;;;;;;;;;;;;;; Data types ;;;;;;;;;;;;;;;;;;;;;;; 

(def topic-title :title)

(def topic-id :id)

;; Type for topic maps.
;; Consists of
;; - topic-graph : Digraph of topics connected with parent-child relations
;; - topic-docs : bipartite Digraph of topics and document ids with topic-docid relations
;; - doc-map : hashmap of IDocument ids to documents
(defrecord TopicMap [topic-graph ^Digraph topic-docs doc-map])

;; remove this comment ;; An example concrete type for documents in the topic map.
;; remove this comment ;; The code of the package relies only on IDocument and wiki_api.core.IDocument.
(defrecord Document [id string]
  ;; IDocument
  ;; (doc-id [this] (:id this))
  ;; wapi/IDocument
  ;; (doc-string [this] (:docstr this))
)

(def doc-id :id)
(def doc-string :string)
(def node-title :title)

;;;;;;;;;;;;;;;;;;;;;; 'Primitive' functions for working with topic-maps  ;;;;;;;;;;;;;;;;;;;;;;; 

(defn get-topics [topic-map]
  (g/get-nodes (:topic-graph topic-map)))

(def get-docs (comp keys :doc-map))

(def n-topics (comp count get-topics))

(def n-docs (comp count :doc-map))

(defn proper-docs [topic-map topic]
  (g/out-links (:topic-docs topic-map) topic))

(defn child-topics [topic-map topic]
  (g/out-links (:topic-graph topic-map) topic))

(defn parent-topics [topic-map topic]
  (g/in-links (:topic-graph topic-map) topic))

;;;;;;;;;;;;;;;;;;;;;; Building the topic map and related algorithms ;;;;;;;;;;;;;;;;;;;;;;; 

;; (defn build-topic-map [docs]
;;   (let [distinct-docs #(->> % (group-by doc-id) vals (map first))
;;         docs (distinct-docs docs)
;;         annotations (wapi/select-max-strength (apply wapi/annotate wminerservice docs))
;;         doc-ids (map doc-id docs)
;;         articles (set (map :article annotations))
;;         article-cats (u/val-map #(wapi/article-categories wminer/service %) articles) 
;;         cats (set (apply concat (vals article-cats)));
;;         cat-rels (map reverse (apply wapi/cat-relations wminer/service cats));
;;         topics (concat articles cats);
;;         topic-links (concat cat-rels
;;                             (for [[article cats] article-cats
;;                                   cat cats]
;;                               [cat article]))
;;         topic-graph (-> (g/digraph)
;;                       (g/add-nodes topics)
;;                       (g/add-links topic-links))
;;         topic-doc-links (map (juxt :article (comp doc-id :doc)) annotations)
;;         topic-docs (-> (g/digraph)
;;                      (g/add-nodes articles)
;;                      (g/add-nodes doc-ids)
;;                      (g/add-links topic-doc-links))]
;;     (TopicMap. topic-graph topic-docs (zipmap doc-ids docs))))

(defn init-topic-map
  "Initializes the topic map with the supplied documents."
  [docs]
  (let [distinct-docs #(->> % (group-by doc-id) vals (map first))
        docs (distinct-docs docs)
        doc-ids (map doc-id docs)
        topic-graph (-> (g/digraph))
        topic-docs (-> (g/digraph)
                     (g/add-nodes doc-ids))]
    (TopicMap. topic-graph topic-docs (zipmap doc-ids docs))))

(defn link-to-articles
  "'Updates' the topic map by linking documents to Wikipedia articles."
  [topic-map]
  (let [{:keys [topic-graph topic-docs doc-map]} topic-map
        docs (vals doc-map)
        doc-ids (mapv doc-id docs)
        doc-strings (mapv doc-string docs)
        annotations (map wapi/select-max-strength
                         (apply wapi/annotate wminer/service doc-strings))
        report-doc (fn [doc annotations]
                     (str (if (:abstract doc) 1 0) ":" (count annotations)))
        _ (println "Document statistics: " (mapv report-doc docs annotations))
        articles (set (mapcat #(map :article %) annotations))
        topic-doc-links ;;(mapcat (juxt :article (comp doc-id :doc)) annotations)
          (for [[doc-id article-links] (map vector doc-ids annotations)
                link article-links]
            [(:article link) doc-id])]
    (-> topic-map
      (update-in [:topic-graph] g/add-nodes articles)
      (update-in [:topic-docs] g/add-nodes articles)
      (update-in [:topic-docs] g/add-links topic-doc-links))))

(defn remove-singleton-articles
  "Removes topics (articles) that occur only in 1 document."
  [topic-map]
  (let [singleton-articles (->> (get-topics topic-map)
                                (filter #(= 1 (count (proper-docs topic-map %)))))]
    ;; (println "Singleton articles: " (map :title singleton-articles))
    (-> topic-map
        (update-in [:topic-graph] g/remove-nodes-safe singleton-articles)
        (update-in [:topic-docs] g/remove-nodes-safe singleton-articles))))

(defn retrieve-categories
  "Retrieves the parent categories for the articles in the topic map."
  [topic-map]
  (let [{:keys [topic-graph topic-docs doc-map]} topic-map
        articles (->> (g/get-nodes topic-graph) (filter wapi/article?))
        article-cats (u/val-map #(wapi/article-categories wminer/service %) articles) 
        cats (set (apply concat (vals article-cats)))
        topic-links (for [[article cats] article-cats
                          cat cats]
                      [cat article])]
    (-> topic-map
      (update-in [:topic-graph] g/add-nodes cats)
      (update-in [:topic-graph] g/add-links topic-links))))


(defn link-categories
  "Links categories in the topic map with parent-child relations."
  [topic-map]
  (let [{:keys [topic-graph topic-docs doc-map]} topic-map
        cats (->> (g/get-nodes topic-graph) (filter wapi/category?))
        cat-rels (map reverse (apply wapi/cat-relations wminer/service cats))]
    (-> topic-map
      (update-in [:topic-graph] g/add-links cat-rels))))


(defn merge-topics
  "Merges two topics into one, correctly reassigning the documets and updating parent-child topic links."
  [topic-map topic-keep topic-remove]
  (-> topic-map
    (update-in [:topic-graph] g/merge-nodes topic-keep topic-remove)
    (#(cond-> %
      (g/contains (:topic-docs %) topic-remove)
      (update-in [:topic-docs] g/replace-node topic-remove topic-keep)))
    (update-in [:merged-topics topic-keep] #(if (nil? %) #{topic-remove} (conj % topic-remove)))))

(defn merge-similar
  "Merges child-parent topics whose titles are equal up to stemming, e.g. 'compilers' -> 'compiler'"
  [topic-map]
  (let [{:keys [topic-graph topic-docs]} topic-map
        stem-title (comp t/stem t/string->tokens topic-title)
        similar-titles? #(= (stem-title %1) (stem-title %2))
        pairs-to-merge (for [parent (g/get-nodes topic-graph)
                             child (g/out-links topic-graph parent)
                             :when (similar-titles? parent child)]
                           [parent child])]
    (reduce (partial apply merge-topics) topic-map pairs-to-merge)))

(defn remove-orphan-topics
  "Removes topics that have no douments in them and their subtopic hierarchy.
   Such topics may appear after breaking the loops in the topic map."
  [topic-map]
  (let [{:keys [topic-graph topic-docs]} topic-map
        all-topics (g/get-nodes topic-graph)
        no-docs? #(empty? (g/out-links topic-docs %))
        topics-no-docs (set (filter no-docs? all-topics))
        children-empty? #(every? topics-no-docs (g/out-links topic-graph %))
        topics-to-remove (filter children-empty? topics-no-docs)
        ]
    (-> topic-map
      (update-in [:topic-graph] g/remove-nodes-safe topics-to-remove)
      (update-in [:topic-docs] g/remove-nodes-safe topics-to-remove))))

(defn break-loops
  "Breaks the loops in the topic map."
  [topic-map]
  (-> topic-map
    (update-in [:topic-graph] g/break-loops)
    remove-orphan-topics))

(defn main-topic [topic-map]
  (let [{:keys [topic-graph topic-docs]} topic-map
        all-nodes (g/get-nodes topic-graph)
        npapers (comp count (partial g/out-links topic-docs))]
    (apply max-key npapers all-nodes)))

(defn expand-main-topic [topic-map]
  (let [{:keys [topic-graph topic-docs main-topic]} topic-map
        topics (g/get-nodes topic-graph)
        ancestorz (set (g/reachable topic-graph [main-topic] :direction :backward))
        ;; _ (println ancestorz)
        wiki-out-link-titles (set (wminer/topic-out-links (topic-title main-topic)))
        out-link-topics (filter (comp wiki-out-link-titles topic-title) topics)
        new-out-links (->> out-link-topics
                        (remove ancestorz)
                        (map #(vector main-topic %)))]
;;    (println "!! Main topic:" main-topic)
    (update-in topic-map [:topic-graph] g/add-links-safe new-out-links)))

(defn- merged-topic-doc-graph* [topic-map]
  (g/cache-closure (g/merge-graphs (:topic-graph topic-map) (:topic-docs topic-map))))

(defn cache-merged [topic-map]
  (assoc topic-map :merged (merged-topic-doc-graph* topic-map)))

(defn cache-topic-dist [topic-map]
  (update-in topic-map [:topic-graph] g/cache-distances))

(defn- merged-topic-doc-graph [topic-map]
  (or (:merged topic-map) (merged-topic-doc-graph* topic-map)))

(defn submap [topic-map topics & more]
  (let [{:keys [topic-graph topic-docs doc-map]} topic-map
        merged-graph (merged-topic-doc-graph topic-map)
        doc-nodes (set (keys doc-map))
        nodes-to-keep (concat topics doc-nodes)
        subgraph (g/subgraph-reduced merged-graph nodes-to-keep)
        new-topic-graph (g/subgraph subgraph topics)
        doc-containing-topics (mapcat #(g/in-links subgraph %) doc-nodes)
        topic-doc-nodes (distinct (concat doc-nodes doc-containing-topics))
        new-topic-docs (g/subgraph subgraph topic-doc-nodes)
        between-topic-links (->> (g/get-links new-topic-docs)
                              (remove (comp doc-nodes second)))
        new-topic-docs (g/remove-links new-topic-docs between-topic-links)
        in-topic-docs (set (mapcat #(g/out-links new-topic-docs %) doc-containing-topics))
        orphan-docs (remove in-topic-docs doc-nodes)
        new-topic-docs (g/remove-nodes-safe new-topic-docs orphan-docs)
        new-doc-map (apply dissoc doc-map orphan-docs)]
    (merge 
      (TopicMap. new-topic-graph new-topic-docs new-doc-map)
      (select-keys topic-map (:keep (apply hash-map more))))))

;;;;;;;;;;;;;;;;;;;;;; Drawing the topic map ;;;;;;;;;;;;;;;;;;;;;;; 

(defn cum-freqs [topic-map]
  (let [{:keys [topic-graph topic-docs]} topic-map
        topic-freq (fn [node]
                     (->> (g/reachable topic-graph [node])
                          (mapcat (partial g/out-links topic-docs))
                          distinct count))]
    (u/val-map topic-freq (g/get-nodes topic-graph))))

(defn freq-based-font-fn [freqs]
  (let [freq-scale (distinct (vals freqs))
        minfreq (apply min freq-scale)
        maxfreq (apply max freq-scale)
        minfont 10 
        maxfont 20
        freqrange (max (- maxfreq minfreq) 1)
        fontrange (- maxfont minfont)]
    (fn freq-based-font [topic]
      (-> (get freqs topic)
        (* 1.0)
        (-  minfreq)
        (/ freqrange)
        (* fontrange)
        (+ minfont)
        (Math/round)))))

(defn freq-based-name-fn [freqs]
  (fn [topic]
    (str (node-title topic) " (" (freqs topic) ")")))

(defn graph2svg [graph & more]
  (dot/dotstr2svg (apply g/graph2dot graph more) :dot))

(defn topics2svg [topic-map & {:as opt}]
  (let [freqs (cum-freqs topic-map)
        font-fn (freq-based-font-fn freqs)
        default-opt {:id-fn :id
                     :name-fn (freq-based-name-fn freqs)
                     :font-fn (freq-based-font-fn freqs)}]
    (apply graph2svg (:topic-graph topic-map)
           (apply concat (merge default-opt opt)))))

(defn display-topics [topic-map]
  (dot/show-svg (topics2svg topic-map)))

;;;;;;;;;;;;;;;;;;;;;; Temporary helper functions for playing in repl ;;;;;;;;;;;;;;;;;;;;;;; 

(defn prepare-map [docs]
  (let [time (System/currentTimeMillis)]
    (-> docs
        (doto (#(println (count %) "results")))
        init-topic-map
        link-to-articles
        (doto (#(println (count (get-topics %)) "articles in" (/ (- (System/currentTimeMillis) time) 1000.0))))
        ;; remove-singleton-articles
        ;; (doto (#(println (count (get-topics %)) "articles after removing singletons")))
        retrieve-categories
        (doto (#(println (count (get-topics %)) "articles and categories")))
        link-categories
        (doto ((fn [_] (println "categories linked"))))
        merge-similar
        (doto (#(println (count (get-topics %)) "articles and categories after merge")))
        break-loops
        (doto ((fn [_] (println "cycles broken"))))
        (u/assocf main-topic identity :main-topic)
        (doto (#(println "Main topic:" (:main-topic %))))
        expand-main-topic
        (#(submap % (get-topics %)))
        (doto ((fn [_] (println "Computed the submap")))))))


