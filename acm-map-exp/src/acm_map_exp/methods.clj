(ns acm-map-exp.methods
  (:use [acm-map-exp.acm :as acm])
  (:require [clojure.string :as string] 
            [acm-map-exp.util :as u]
            [wiki-api.core :as wapi]
            [wminer.core :as wminer]))


;;;; Mapping to Wikipedia ;;;;

;; ::wiki-link is (mk-wiki-link String wiki-api.core.Article)
(defn mk-wiki-link
  "Mapping from a label to Wikipedia article."
  [label article]
  ^{:type ::wiki-link}
   {:label label :article article})

;; :concept -> seq[::wiki-link]
(defn map2wiki*
  "Maps the concept to Wikipedia articles by resolving its preferred and
   alternative titles as anchor texts in inter-article links. Returns the
   sequence of label-article mappings sorted in the decreasing order of probability."
  [concept opt]
  (let [labels (cons (pref-label concept) (alt-labels concept))
        mappings (for [label labels
                       article (wapi/search* wminer/service label opt)]
                   (mk-wiki-link label article))]
    (sort-by (comp - :prob :article)
             mappings)))

;; :concept -> ISeq[:simple-mapping]
(defn map2wiki [concept & {:as opt}]
  "Same as map2wiki* but with variadic options."
  (map2wiki* concept opt))

