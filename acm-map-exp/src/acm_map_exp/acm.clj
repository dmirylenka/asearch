(ns acm-map-exp.acm
  (:use [clojure.zip :only [xml-zip]]
        [clojure.data.zip.xml]
        [clojure.pprint :only [print-table]])
  (:require [clojure.zip :as zip]
            [clojure.xml :as xml]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [acm-map-exp.util :as u]))

;;;;  ACM CSS taxonomy ;;;;

;; ::concept is a data type for the concept in ACM CCS taxonomy
;; ::concept is currently represented as an xml element,
;;           and allows methods listed below.

;; ::conf is a map

;; ::acm is a whole xml document

;; ::conf -> acm
(defn read-acm [conf]
  (xml/parse (conf :acm-file)))

;; ::acm -> seq[::concept]
(defn all-concepts [acm]
  (-> acm xml-zip (xml-> :skos:Concept zip/node)))

(defn- test-attr [attr-name pred]
  (fn [loc] (when (pred (attr loc attr-name)) true)))

;; ::acm -> seq[::concept]
(defn top-concepts [acm]
  (let [zipper (xml-zip acm)
        ids (set (xml-> zipper :skos:ConceptScheme :skos:hasTopConcept (attr :rdf:resource)))]
    (xml-> zipper :skos:Concept (test-attr :rdf:about ids) zip/node)))

;; ::concept -> String
(defn pref-label
  "Preferred label of the concept."
  [concept]
  (xml1-> (xml-zip concept) :skos:prefLabel text))

;; ::concept -> String
(defn concept-id
  "Id of the concept."
  [concept]
  (.substring (xml1-> (xml-zip concept) (attr :rdf:about)) 1))

;; ::concept -> String
(defn alt-labels
  "Alternative labels of the concept."
  [concept]
  (xml-> (xml-zip concept) :skos:altLabel text))

;; ::concept -> set[String]
(defn child-ids
  "Ids of the narrower concepts."
  [concept]
  (set (xml-> (xml-zip concept) :skos:narrower (attr :rdf:resource))))

;; ::concept -> set[String]
(defn parent-ids
  "Ids of the broader concepts."
  [concept]
  (set (xml-> (xml-zip concept) :skos:broader (attr :rdf:resource))))

;; ::concept -> seq[::concept]
(defn narrower
  "Narrower concepts."
  [acm concept]
  (let [ids (child-ids concept)]
    (xml-> (xml-zip acm) :skos:Concept (test-attr :rdf:about ids) zip/node)))

;; ::concept -> seq[::concept]
(defn broader
  "Broader concepts."
  [acm concept]
  (let [ids (parent-ids concept)]
    (xml-> (xml-zip acm) :skos:Concept (test-attr :rdf:about ids) zip/node)))

;; ::concept -> Boolean
(def leaf?
  "Tests whether the concept is a leaf node in the taxonomy."
  (comp empty? child-ids))

;; ::concept -> Boolean
(def top?
  "Tests whether the concept is a top concept in the taxonomy."
  (comp empty? parent-ids))

;; ::acm -> seq[::concept]
(defn leaves
  "Returns the leaf nodes of the taxonomy."
  [acm]
  (filter leaf? (all-concepts acm)))

;; String -> seq[::concept]
(defn by-pref-label
  "Find concepts by the preferred label."
  [acm label]
  (filter #(= (pref-label %) label) (all-concepts acm)))

;; String -> ::concept
(defn by-pref-label1
  "Finds the concept by the preferred label.
   Throws an exception if not found of found more than one."
  [acm label]
  (let [concepts (by-pref-label acm label)
        error #(throw (Exception. %))]
    (cond (empty? concepts) (error (str "No concepts for label: " label))
          (> (count concepts) 1) (error (str "More than one concept for label: " label))
          :else (first concepts))))

