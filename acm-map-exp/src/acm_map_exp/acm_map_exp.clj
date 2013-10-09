(ns acm-map-exp.acm-map-exp
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [acm-map-exp.acm :as acm]
            [acm-map-exp.experiment :as exp]
            [acm-map-exp.methods :as algo]
            [acm-map-exp.util :as u])
  (:import [java.util Date]))

;; ::conf is a map

;; ::conf
(def acm-conf
  ^{:type ::conf}
   {:acm-file "/Users/dmirylenka/data/dbpedia/ACMCCS.rdf.xml"
    :rand-seed 751881
    :sample-size 100})

;; ::conf -> ::conf
(defn new-conf*
  "Creates new configuration merting the provided options onto the default configuration."
  [opt]
  (with-meta
    (-> exp/default-conf
        (merge acm-conf)
        (merge opt)
        (assoc :date-time (Date.)))
    {:type ::conf}))

;; ::conf -> ::conf
(defn new-conf
  "Same as new-conf* only with variadic options."
  [& {:as opt}]
  (new-conf* opt))

;; ::acm-input-data = map(acm,concepts,top,leaves)
(defn mk-acm-input-data
  "Creates the input data for experiments with ACM:
   acm taxonomy, all its concepts, just top concepts, and just leaves."
  [conf]
  (let [acm (acm/read-acm conf)
        concepts (acm/all-concepts acm)
        top (acm/top-concepts acm)
        leaves (filter acm/leaf? concepts)]
    ^{:type ::acm-input-data}
     {:acm acm
      :concepts concepts
      :top top
      :leaves leaves}))

;; ::descr-data[T] is (mk-descr-data String T)
(defn mk-descr-data
  "A piece of data with associated description."
  [descr data]
  ^{:type ::descr-data}
   {:descr descr
    :data data})

;; ::concepts->wiki is map[:acm/concept seq[:algo/wiki-link]]

;; ::acm-input-data -> ::descr-data[concepts->wiki]
(defn map2wiki-label-simple-search
  "Maps concepts from ACM CSS to Wikipedia articles by their preferred and alternative labels.
   Uses simple search algorithm of Wikipedia Miner (matching the whole input string to the article labels (title, redirect and anchor texts).
   As data, returns the map (concept -> label-article mappings)."
  [input-data conf]
  (let [{:keys [concepts]} input-data
        mapping (u/val-map algo/map2wiki concepts)]
    (mk-descr-data
     "Mapping from ACM CCS concepts to Wikipedia articles.
      Each concept has a (possibly empty) list label->article links."
     mapping)))

;; ::stats[T] = map[Keyword,T]

;; ::descr-data -> ::descr-data[::stats[::concept->wiki]]
(defn mapped-failed-stats 
  "Splits the mapping of concepts to wikipedia according to whether there is at least
   one successfull mapping."
  [raw-data conf]
  (let [mapping (-> raw-data :data)
        mapped? #(seq (second %))
        mapped (into {} (filter mapped? mapping))
        failed (into {} (remove mapped? mapping))]
    (mk-descr-data
     "ACM CCS concepts split according to whether they have been successfully mapped to Wikipedia (at least one link) or not (zero links). Additional statistics :nmapped - the number of successfully linked concepts, :nfailed - the number of concepts for which the mapping gave no results."
     ^{:type ::stats}
      {:mapped mapped
       :failed failed
       :nmapped (count mapped)
       :nfailed (count failed)})))

;; ::report is map[Keyword,::descr-data[vec[map[Keyword,String]]]]

;; ::descr-data -> ::report
(defn report-sample-mappings
  "Generates a report with:
   * a number of successful and unsuccessful mappings from ACM CCS concepts to Wikipedia articles,
   * examples (random samples) of successful and unsuccessful mappings."
  [stats conf]
  (let [{:keys [mapped failed nmapped nfailed]}  (:data stats)
        {:keys [rand-seed sample-size]} conf
        rand-int-gen (u/new-rand-int-gen rand-seed)
        nmapped-stats [{:nmapped nmapped :nfailed nfailed
                        :percent-mapped (/ nmapped (+ nmapped nfailed 0.0))}]
        mapped-sample (mapv (fn [[concept links]]
                             {:concept (acm/pref-label concept)
                              :label (:label (first links))
                              :article (:title (:article (first links)))})
                           (u/rand-take sample-size mapped rand-int-gen))
        failed-sample (mapv (fn [[concept links]]
                             {:concept (acm/pref-label concept)})
                           (u/rand-take sample-size failed rand-int-gen))]
    ^{:type ::report}
     {:nmapped-stats
      (mk-descr-data
       "Statistics on the concepts mapped with simple title search method."
       nmapped-stats)
      :mapped-sample
      (mk-descr-data
       "Concepts successfully mapped with the corrersponding most likely articles."
       mapped-sample)
      :failed-sample
      (mk-descr-data
       "Concepts that did not map."
       failed-sample)}))

(defn map2wiki-simple-search-run-2013-07-10-0 []
  (let [experiment {:generate-data map2wiki-label-simple-search
                    :compute-stats mapped-failed-stats
                    :generate-report report-sample-mappings}
        conf (new-conf :exp-name "map2wiki-simple-search-2013-07-10-0"
                       :file-name-fn :type-suffix
                       :description 
"Map preferred and alternative labels of ACM concepts to Wikipedia using the simple search mechanism of Wikipedia Miner, that tries to resolve the whole string as a Wikipedia article label (title, redirect or anchor text). Run Wikipedia Miner with default parameters. Compute the number of successful (at least one article) and unsuccessful mappings. Display the titles of 100 random successfully mapped concepts with the most likely article and the concept label that mapped to it. Display the titles of 100 random unsucessfully mapped concepts.")
        input-data (mk-acm-input-data conf)]
    (exp/run-experiment experiment input-data conf)))

;; (defn rewrite-report []
;;   (let [stats-file "./resources/data/exp/map2wiki-simple-search-2013-07-10-0/stats.clj"
;;         report-file "./resources/data/exp/map2wiki-simple-search-2013-07-10-0/report.clj_new_new"
;;         stats (exp/read-from-file stats-file)
;;         conf (new-conf)
;;         report (report-sample-mappings stats conf)]
;;     (exp/save-to-file report report-file)))


;; ::string -> vector[::mapping]
(defn acm-dbpedia-mapping [input-data conf]
  (let [file-name (:mapping-file conf)
        lines (string/split (slurp file-name) #"\n")
        pattern #"^.*#(\d+)\|(.*?)\|.*\|(.*)\|.*$"
        concepts (:concepts input-data)
        id->concept (->> concepts
                      (u/key-map acm/concept-id))
        mk-category (fn [title] {:title title})
        line->mapping #(let [[acm-id cat-name prob] (rest (re-find pattern %))
                             concept (id->concept acm-id)]
                         [concept
                          (assoc
                              (algo/mk-wiki-link
                               (acm/pref-label concept)
                               (mk-category cat-name))
                            :prob (Double/parseDouble prob))])
        default-empty-maping (zipmap concepts (repeat []))
        successful-mappings (->> lines
                                 (map line->mapping)
                                 (u/group-map first second)
                                 (u/map-val (partial sort-by (comp - :prob))))]
    
    (merge default-empty-maping
           successful-mappings)))

;; ::acm-input-data -> ::descr-data[concepts->wiki]
(defn logmap-acm-wiki-cateories-match [input-data conf]
  (let [mapping (acm-dbpedia-mapping input-data conf)]
    (mk-descr-data
     "Mapping from ACM CCS concepts to Wikipedia categories.
      Each concept has a (possibly empty) list label->category links."
     mapping)))

(defn logmap-match-precision-coverage-2013-08-05-0 []
  (let [experiment {:generate-data logmap-acm-wiki-cateories-match
                    :compute-stats mapped-failed-stats
                    :generate-report report-sample-mappings}
        conf (new-conf :exp-name "logmap-match-precision-coverage-2013-08-05-0"
                       :mapping-file "/Users/dmirylenka/data/dbpedia/logmap-dbpedia-acm-match/logmap2_mappings.txt"
                       :file-name-fn :type-suffix
                       :description 
"Examine the matching between ACM CCS and Wikipedia-based category hierarchy produced by logmap. Wikipedia-based hierarchy is the result of the sparql query to DBPedia that selects the categories within 7 steps down from Computer_science. Every cycle in the obtained hierarchy is detected and merged into a single node. The experiment reports sample successfull mappings as well as non-mappings.")
        input-data (mk-acm-input-data conf)]
    (exp/run-experiment experiment input-data conf)))


(defn logmap-match-precision-coverage-2013-08-07-0 []
  (let [experiment {:generate-data logmap-acm-wiki-cateories-match
                    :compute-stats mapped-failed-stats
                    :generate-report report-sample-mappings}
        conf (new-conf :exp-name "logmap-match-precision-coverage-2013-08-07-0"
                       :mapping-file "/Users/dmirylenka/data/dbpedia/logmap-dbpedia-acm-match-art/logmap2_mappings.txt"
                       :file-name-fn :type-suffix
                       :description 
"Examine the matching between ACM CCS and Wikipedia-based category-article hierarchy produced by logmap. Wikipedia-based hierarchy is obtained by querying DBPedia for the categories within 7 steps down from Computer_science, and articles that belong to them. Every cycle in the obtained hierarchy is detected and merged into a single node. The experiment reports sample successfull mappings as well as non-mappings.")
        input-data (mk-acm-input-data conf)]
    (exp/run-experiment experiment input-data conf)))

(defn -main [args])
