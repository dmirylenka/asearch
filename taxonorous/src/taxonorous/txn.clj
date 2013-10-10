(ns taxonorous.txn
  (:use [clojure.zip :only [xml-zip]]
        [clojure.data.zip.xml])
  (:require [clojure.zip :as zip]
            [clojure.xml :as xml]
            [wiki-api.core :as wapi]
            [wminer.core :as wminer]))

(def conf {:acm-file "./resources/data/ACMCCS.xml"})

(defn read-acm [conf]
  (xml/parse (conf :acm-file)))

(defn all-concepts [acm]
  (-> acm xml-zip (xml-> :skos:Concept zip/node)))

(defn test-attr [attr-name pred]
  (fn [loc] (when (pred (attr loc attr-name)) true)))

(defn top-concepts [acm]
  (let [zipper (xml-zip acm)
        ids (set (xml-> zipper :skos:ConceptScheme :skos:hasTopConcept (attr :rdf:resource)))]
    (xml-> zipper :skos:Concept (test-attr :rdf:about ids) zip/node)))

(defn pref-label [concept]
  (xml1-> (xml-zip concept) :skos:prefLabel text))

(defn alt-labels [concept]
  (xml-> (xml-zip concept) :skos:altLabel text))

(defn child-ids [concept]
  (set (xml-> (xml-zip concept) :skos:narrower (attr :rdf:resource))))

(defn parent-ids [concept]
  (set (xml-> (xml-zip concept) :skos:broader (attr :rdf:resource))))

(defn children [acm concept]
  (let [ids (child-ids concept)]
    (xml-> (xml-zip acm) :skos:Concept (test-attr :rdf:about ids) zip/node)))

(defn parents [acm concept]
  (let [ids (parent-ids concept)]
    (xml-> (xml-zip acm) :skos:Concept (test-attr :rdf:about ids) zip/node)))

(def leaf? (comp empty? child-ids))

(def top? (comp empty? parent-ids))

(defn leaves [acm]
  (filter leaf? (all-concepts acm)))

(def acm (read-acm conf))

(def concepts (all-concepts acm))

(def top (top-concepts acm))

(defn map2wiki* [concept opt]
  (let [labels (cons (pref-label concept) (alt-labels concept))]
    (->> labels
         (mapcat #(wapi/search* wminer/service % opt))
         (sort-by (comp - :prob)))))

(defn map2wiki [concept & {:as opt}]
  (map2wiki* concept opt))

;; (println (-> acm leaves count)) ; 1882

;; (println (->> acm leaves (map map2wiki) (remove empty?) count)) ; 1333

;; (println (-> concepts count)) ; 2299

;; (println (->> concepts (map map2wiki) (remove empty?) count)) ; 1575
