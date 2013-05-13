(ns scaiella12topical.core
  (:require [clojure [string :as string]
                     [set :as set]]
            [utils.core :as u]
            [topic-maps.core :as tmaps]
            [graphs.core :as g]
            [wiki-api.core :as wapi]
            [wminer-api.core :as wminer]
            [tagme-api.core :as tagme]
            [dot-api.core :as dot]
            [clatrix.core :as m]
            [clojure.java [jdbc :as sql]]
            [wikidb [core :as wikidb]
                    [config :as conf]]
            )
  (:import [topic_maps.core Topic]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Clustering text snippets as described here:                          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; "Topical clustering of search results." (Scaiella et al., WSDM 2012) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mk-article [topic-info]
  (tmaps/map->Topic (assoc topic-info :kind ::tmaps/article)))

(defn mk-category [topic-info]
  (tmaps/map->Topic (assoc topic-info :kind ::tmaps/category)))

(extend-type wiki_api.core.IArticle
  g/IGraphNode
  (node-id [this] (wapi/article-id this))
  (node-title [this] (wapi/article-title this)))

;(defrecord DocArticleLink [doc article spot rho])

(defn norm
  [category-name]
  (-> category-name
   #_ (string/trim)
   #_  string/lower-case
    (string/replace #"\s+" "_")
   #_  string/capitalize))

(defn unnorm
  [category-name]
  (-> category-name 
  #_ (string/trim)
  #_ string/lower-case
    (string/replace #"_" " ")
    (string/replace #"\s+" " ")))

(defn article-categories [page]
  (map unnorm (wikidb/page-cats (norm page))))

(defn cat-relations [cats]
  (map #(map unnorm %) (wikidb/cat-relations (map norm cats))))

;(defrecord DocArticleMap [doc-map topic-map doc-topic-links])

(defn map-to-articles [docs]
  (let [mk-distinct-docs #(->> % (group-by tmaps/doc-id) vals (map first))
        docs (mk-distinct-docs docs)]
    (apply wapi/annotate tagme/service docs)))

(defn remove-frequent-articles [max-freq links]
  (let [total-docs (count (distinct (map wapi/link-doc links)))
        max-count (* total-docs max-freq)
        count-docs #(count (distinct (map wapi/link-doc %)))
        counts (->> links
                 (group-by wapi/link-article)
                 (u/map-val count-docs))]
    (remove #(>= (counts (wapi/link-article %)) max-count) links)))

(defn select-significant [links]
  (let [all-articles (set (map wapi/link-article links))
        greedily-iterate
          (fn greedily-iterate [articles-selected links-left articles-left]
            (if (empty? links-left) 
              articles-selected
              (let [volume-map (->> links-left
                             (filter (comp articles-left wapi/link-article))
                             (group-by wapi/link-article)
                             (u/map-val #(apply + (map wapi/link-strength %))))
                    volume #(get volume-map % 0)
                    next-article (apply max-key volume articles-left)
                    new-covered-volume (->> links-left
                                         (filter #(= next-article (wapi/link-article %)))
                                         (map (juxt wapi/link-doc wapi/link-strength))
                                         (into {}))
                    covered-volume #(get new-covered-volume % 0)
                    deduct-covered-volume (fn [links]
                                            (for [link links
                                                  :let [new-rho (- (wapi/link-strength link) (covered-volume (wapi/link-doc link)))]
                                                  :when (> new-rho 0)]
                                              (assoc (wapi/map->DocArticleLink link) :strength new-rho)))]
                (recur (conj articles-selected next-article)
                       (deduct-covered-volume links-left)
                       (disj articles-left next-article)))))
        sign-articles (set (greedily-iterate [] links all-articles))]
    (filter (comp sign-articles wapi/link-article) links)))

(defn- weight-matrix [articles rel-weights epsilon]
  (m/matrix (for [a1 articles]
              (for [a2 articles]
                (if (= a1 a2) 0 
                  (+ (get rel-weights #{a1 a2} epsilon)))))))

(defn m+ [arg & args]
  (if (empty? args)
    arg
    (apply m/+ arg args)))

(defn- degree-matrix [W] ; laplacian
  (->> W m/rows (apply m+) m/dense first m/diag))

(defn- regularize [M alpha]
  (m/+ (m/diag (repeat (m/ncols M) alpha)) M))

(defn- compute-epsilon [rel-weights]
  (->> rel-weights vals (remove zero?) (apply min) (* 1e-6)))

(defn- spectral-view [cluster rel-weights epsilon]
  (let [W (weight-matrix cluster rel-weights epsilon)
        D (degree-matrix W)
        L (m/- D W)]
    (m/eigen L D)))

(defn- graph-cut [node-cluster-1 node-cluster-2 rel-weights epsilon]
  (apply +
    (for [a node-cluster-1 b node-cluster-2]
      (get rel-weights #{a b} epsilon))))

(defn- cluster-volume [node-cluster all-nodes rel-weights epsilon]
  (apply +
     (for [a node-cluster b all-nodes
           :when (not= a b)]
       (get rel-weights #{a b} epsilon))))

(defn- normalized-cut [[node-cluster-1 node-cluster-2] rel-weights epsilon]
  (let [all (concat node-cluster-1 node-cluster-2)
        cut-value (graph-cut node-cluster-1 node-cluster-2 rel-weights epsilon)
        volumes (map #(cluster-volume % all rel-weights epsilon)
                     [node-cluster-1 node-cluster-2])]
    (* cut-value (apply + (map #(/ 1 %) volumes)))))

(defrecord Cluster [items eigen-values eigen-vectors])

(defn- mk-cluster [items rel-weights epsilon big?]
  (let [{:keys [values vectors]}
        (if (big? items)
          (spectral-view items rel-weights epsilon)
          nil)]
    (Cluster. items values vectors)))

(defn- split-sparsest [clusters rel-weights epsilon big?]
  (let [denseness #(second (:eigen-values %))
        sparsest-cluster (apply min-key denseness clusters)
        second-evector (second (m/dense (m/t (:eigen-vectors sparsest-cluster))))
        cluster-items (:items sparsest-cluster)
        point-evector (map vector cluster-items second-evector)
        sorted-points (map first (sort-by second point-evector))
        cut-fn #(normalized-cut (split-at % sorted-points) rel-weights epsilon)
        best-cut (apply min-key cut-fn (range 1 (count cluster-items)))
        mk-cluster* #(mk-cluster % rel-weights epsilon big?)
        subclusters (map mk-cluster* (split-at best-cut sorted-points))]
    [sparsest-cluster subclusters]))

(defn- display-clusters [clusters rel-weights]
  (let [all-nodes (apply concat clusters)
        relations (keys rel-weights)
        graph (-> (g/undigraph)
                (g/add-nodes all-nodes)
                (g/add-links relations))
        weight-fn #(-> % set rel-weights (* 100) Math/round)
        cluster-idx (into {} (for [[cluster idx] (map vector clusters (range))
                                   node cluster]
                               [node idx]))
        color-fn (g/diverse-colors cluster-idx (range (count clusters)))]
    (->
      (g/graph2dot graph :undirected true :weight weight-fn :color color-fn)
      (dot/dotstr2svg :fdp)
      dot/show-svg)))

(defn topic-clusters [min-cluster-size max-n-clusters links]
  (let [articles (vec (distinct (map wapi/link-article links)))
        article-docs (u/group-map wapi/link-article wapi/link-doc links)
        article-pairs (for [a1 articles a2 articles :let [pair [a1 a2]] :when (apply < (map wapi/article-id pair))] pair)
        rel-scores (apply wapi/relatedness tagme/service article-pairs)
        rel-weights (into {} (map (juxt wapi/rel-articles wapi/rel-strength) rel-scores))
        epsilon (compute-epsilon rel-weights)
        ;big? #(>= (count (:items %)) (* 2 min-cluster-size))
        count-docs (fn [items] (count (distinct (mapcat article-docs items))))
        big? #(and (> (count-docs %)  min-cluster-size)
                   (> (count %) 1))
        big-cluster? (comp big? :items)
        mk-clustering #(map :items %) 
        iterate-split-sparsest
          (fn [small-clusters big-clusters]
            (if (or (empty? big-clusters)
                    (>= (+ (count big-clusters)
                           (count small-clusters))
                        max-n-clusters))
              (mk-clustering (into big-clusters small-clusters))
              (let [[sparsest subclusters] (split-sparsest big-clusters rel-weights epsilon big?)
                    small-clusters (-> small-clusters (into (remove big-cluster? subclusters)))
                    big-clusters (-> big-clusters (disj sparsest) (into (filter big-cluster? subclusters)))]
        ;        (display-clusters (mk-clustering (into big-clusters small-clusters)) rel-weights)
                (recur small-clusters big-clusters))))
        init-clustering #{(mk-cluster articles rel-weights epsilon big?)}]
    (iterate-split-sparsest #{} init-clustering)))

(defn label-cluster [articles links]
  (let [articles (set articles)]
    (->> links
      (filter (comp articles wapi/link-article))
      (group-by wapi/link-article)
      (u/map-val #(apply + (map wapi/link-strength %)))
      (apply max-key second)
      first)))

(defn- build-topic-rels [articles]
  (let [topic-map (u/key-map wapi/article-title articles)
        mk-article #(topic-map %)
        mk-category #(tmaps/->Topic ::tmaps/category %)
        article-titles (set (keys topic-map))
        article-cats (u/val-map article-categories article-titles)
        ;_ (println "articles with no categories:" (filter (comp empty? second) article-cats))
        cat-art-rels (for [[article categories] article-cats
                           category categories]
                       [category article])
        all-titles (distinct (apply concat cat-art-rels))
        cat-rels (map (partial map mk-category) (map reverse (cat-relations all-titles)))
        ;_ (println "cat-rels:" cat-rels)
        ]
    (->> (map (fn [[cat art]] [(mk-category cat) (mk-article art)]) cat-art-rels)
      (concat cat-rels)
      distinct)))

(defn build-topic-map [docs & {:as opt}]
  (sql/with-connection conf/wiki-db
    (let [distinct-docs #(->> % (group-by tmaps/doc-id) vals (map first))
          docs (distinct-docs docs)
          links (->> docs
                  map-to-articles
                  wapi/select-max-strength
                  select-significant)
          cluster-size (or (:cluster-size opt) 3)
          nclusters (or (:nclusters opt) 8)
          nmerge (or (:nmerge opt) 4)
          clusters (topic-clusters cluster-size (dec (+ nclusters nmerge)) links)
          [clusters other] (split-at (dec nclusters) (sort-by (comp - count) clusters))
          clusters (cond-> clusters
                           (seq other) (conj (apply concat other)))
          doc-ids (map tmaps/doc-id docs)
          topic-doc-id-map (->> links
                          (group-by wapi/link-article)
                          (u/map-val #(map (comp tmaps/doc-id wapi/link-doc) %)))
          topic-doc-links (for [cluster clusters
                                :let [article (label-cluster cluster links)]
                                doc (set (mapcat topic-doc-id-map cluster))]
                            [article doc])
          articles (distinct (map wapi/link-article links))
          topic-links (build-topic-rels articles)
          all-topics (distinct (apply concat topic-links))
          topic-graph (-> (g/digraph)
                        (g/add-nodes all-topics)
                        (g/add-links topic-links))
          topic-docs (-> (g/digraph)
                      (g/add-nodes all-topics)
                      (g/add-nodes doc-ids)
                      (g/add-links topic-doc-links))
          topic-map (tmaps/->TopicMap topic-graph topic-docs (zipmap doc-ids docs))
          topic-map (-> topic-map
                      tmaps/merge-similar
                      (update-in [:topic-graph] g/break-loops)
                      tmaps/remove-orphan-topics
                      (u/assocf tmaps/main-topic identity :main-topic)
                      tmaps/expand-main-topic)
          ;_ (println (g/get-nodes (:topic-graph topic-map)))
          ;_ (tmaps/display-topics topic-map)
          {:keys [topic-graph topic-docs]} topic-map
          topics-with-docs (->> (g/get-nodes topic-graph)
                             (remove (comp empty? (partial g/out-links topic-docs))))
          ]
      (tmaps/submap topic-map topics-with-docs))))
    ; (remove-frequent-articles 0.5)
