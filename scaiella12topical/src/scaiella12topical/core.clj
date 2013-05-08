(ns scaiella12topical.core
  (:require [clojure [string :as string]
                     [set :as set]]
            [utils.core :as u]
            [topic-maps.core :as tmaps]
            [graphs.core :as g]
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

(defrecord DocArticleLink [doc article spot rho])

(defrecord DocArticleMap [doc-map topic-map doc-topic-links])

(defn map-to-articles [docs]
  (let [mk-distinct-docs #(->> % (group-by tmaps/doc-id) vals (map first))
        docs (mk-distinct-docs docs)
        article->topic #(mk-article (select-keys % [:id :title]))]
    (for [doc docs
          article (tagme/tagme-annotate-one (tmaps/doc-string doc))]
      (DocArticleLink. doc (article->topic article)
                       (:spot article) (:rho article)))))

(defn select-max-rho [links]
  (let [sort-fn #(sort-by (comp - :rho) %)]
    (->> links
      (group-by (juxt (comp :id :article) (comp :id :doc)))
      (u/map-val sort-fn)
      (u/map-val first)
      vals)))

(defn remove-frequent-articles [max-freq links]
  (let [doc-id (comp :id :doc)
        total-docs (count (distinct (map doc-id links)))
        max-count (* total-docs max-freq)
        count-docs #(count (distinct (map doc-id %)))
        counts (->> links
                 (group-by :article)
                 (u/map-val count-docs))]
    (remove #(>= (counts (:article %)) max-count) links)))

(defn select-significant [links]
  (let [all-articles (set (map :article links))
        greedily-iterate
          (fn greedily-iterate [articles-selected links-left articles-left]
            (if (empty? links-left) 
              articles-selected
              (let [volume-map (->> links-left
                             (filter (comp articles-left :article))
                             (group-by :article)
                             (u/map-val #(apply + (map :rho %))))
                    volume #(get volume-map % 0)
                    next-article (apply max-key volume articles-left)
                    new-covered-volume (->> links-left
                                         (filter #(= next-article (:article %)))
                                         (map (juxt :doc :rho))
                                         (into {}))
                    covered-volume #(get new-covered-volume % 0)
                    deduct-covered-volume (fn [links]
                                            (for [link links
                                                  :let [new-rho (- (:rho link) (covered-volume (:doc link)))]
                                                  :when (> new-rho 0)]
                                              (assoc link :rho new-rho)))]
                (recur (conj articles-selected next-article)
                       (deduct-covered-volume links-left)
                       (disj articles-left next-article)))))
        sign-articles (set (greedily-iterate [] links all-articles))]
    (filter (comp sign-articles :article) links)))

(defn- weight-matrix [articles rel-weights epsilon]
  (m/matrix (for [a1 articles]
              (for [a2 articles]
                (if (= a1 a2) 0 
                  (+ (get rel-weights #{a1 a2} epsilon)))))))

(defn- degree-matrix [W] ; laplacian
  (->> W m/rows (apply m/+) m/dense first m/diag))

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

(defn- mk-cluster [items rel-weights epsilon]
  (let [{:keys [values vectors]} (spectral-view items rel-weights epsilon)]
    (Cluster. items values vectors)))

(defn- split-sparsest [clusters rel-weights epsilon min-cluster-size]
  (let [;spectral-views (mapv #(spectral-vew % rel-weights epsilon) clusters)
        ;denseness (->> % (get spectral-views) :values second)
        denseness #(second (:eigen-values %))
        ;sparsest-idx (apply min-key denseness (range (count clusters)))
        ;sparsest-cluster (get clusters sparsest-idx)
        sparsest-cluster (apply min-key denseness clusters)
        second-evector (second (m/dense (m/t (:eigen-vectors sparsest-cluster))))
        cluster-items (:items sparsest-cluster)
        point-evector (map vector cluster-items second-evector)
        sorted-points (map first (sort-by second point-evector))
        cut-fn #(normalized-cut (split-at % sorted-points) rel-weights epsilon)
        ;best-cut (apply min-key cut-fn (range min-cluster-size (inc (- (count cluster-items) min-cluster-size))))
        best-cut (apply min-key cut-fn (range 1 (count cluster-items)))
        mk-cluster* #(mk-cluster % rel-weights epsilon)
        subclusters (map mk-cluster* (split-at best-cut sorted-points))]
    [sparsest-cluster subclusters]))

(defn- display-clusters [clusters rel-weights ]
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
  (let [articles (vec (distinct (map :article links)))
        article-ids (map :id articles)
        article-map (u/key-map :id articles) 
        id-pairs (for [id1 article-ids
                       id2 article-ids
                       :when (> id2 id1)]
                   [id1 id2])
        rel-scores (tagme/relatedness id-pairs)
        rel-weights (into {} (for [[[id1 id2] score] rel-scores]
                              [#{(article-map id1) (article-map id2)} score]))
        epsilon (compute-epsilon rel-weights)
        big? #(>= (count (:items %)) (* 2 min-cluster-size))
        big? #(> (count (:items %))  min-cluster-size)
        mk-clustering #(map :items %) 
        iterate-split-sparsest
          (fn [small-clusters big-clusters]
            (if (or (empty? big-clusters)
                    (>= (+ (count big-clusters)
                           (count small-clusters))
                        max-n-clusters))
              (mk-clustering (into big-clusters small-clusters))
              (let [[sparsest subclusters] (split-sparsest big-clusters rel-weights epsilon min-cluster-size)
                    small-clusters (-> small-clusters (into (remove big? subclusters)))
                    big-clusters (-> big-clusters (disj sparsest) (into (filter big? subclusters)))]
                (display-clusters (mk-clustering (into big-clusters small-clusters)) rel-weights)
                (recur small-clusters big-clusters))))
        init-clustering #{(mk-cluster articles rel-weights epsilon)}]
    (iterate-split-sparsest #{} init-clustering)))

(defn label-cluster [articles links]
  (let [articles (set articles)]
    (->> links
      (filter (comp articles :article))
      (group-by :article)
      (u/map-val #(apply + (map :rho %)))
      (apply max-key second)
      first)))

(defn- build-topic-rels [articles]
  (let [topic-map (u/key-map :title articles)
        mk-topic #(or (topic-map %)
                      (tmaps/->Topic ::tmaps/category %))
        article-titles (set (keys topic-map))
        article-cats (u/val-map tmaps/article-categories article-titles)
        cat-art-rels (for [[article categories] article-cats
                           category categories
                           :when (not= article category)]
                       [category article])
        all-titles (distinct (apply concat cat-art-rels))
        cat-rels (map reverse (tmaps/cat-relations all-titles))]
    (->> cat-art-rels
      (concat cat-rels)
      distinct
      (map (partial map mk-topic)))))

(defn build-topic-map [docs]
  (sql/with-connection conf/wiki-db
    (let [distinct-docs #(->> % (group-by tmaps/doc-id) vals (map first))
          docs (distinct-docs docs)
          links (->> docs
                  map-to-articles
                  select-max-rho
                  select-significant)
          clusters (topic-clusters 3 12 links)
          doc-ids (map tmaps/doc-id docs)
          topic-doc-id-map (->> links
                          (group-by :article)
                          (u/map-val #(map (comp tmaps/doc-id :doc) %)))
          topic-doc-links (for [cluster clusters
                                :let [article (label-cluster cluster links)]
                                doc (set (mapcat topic-doc-id-map cluster))]
                            [article doc])
          articles (distinct (map :article links))
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
                      tmaps/remove-orphan-topics)
          _ (tmaps/display-topics topic-map)
          {:keys [topic-graph topic-docs]} topic-map
          topics-with-docs (->> (g/get-nodes topic-graph)
                             (remove (comp empty? (partial g/out-links topic-docs))))
          ]
      (tmaps/submap topic-map topics-with-docs))))
    ; (remove-frequent-articles 0.5)
