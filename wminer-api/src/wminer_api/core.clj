(ns wminer-api.core
  (:require [utils.core :as u]
            [clojure [string :as string]]
            [clj-http.client :as http]))

(def ^:private 
  wminer-urls {
    :waikato "http://wikipedia-miner.cms.waikato.ac.nz/services/wikify"
    :lion "http://lion0b.disi.unitn.it:8082/wikipedia-miner/services/wikify"})

(def default-params 
  {:minProbability 0.0
   :repeatMode "all"
   :linkFormat "wiki"
   :responseFormat "direct"})

(def param-mapping
  {:min-prob :minProbability
   :repeat-mode :repeatMode
   :link-format :linkFormat
   :response-format :responseFormat
   :source :source})

(defn wiki-articles [snippets & more]
  (let [opt (apply hash-map more)
        delim " #@ "
        query (string/join delim snippets)
        custom-params (u/map-key param-mapping (select-keys opt (keys param-mapping)))
        request {:form-params (-> default-params
                                (merge custom-params)
                                (assoc :source query))}
        wminer-url-key (or (:wminer opt) :lion)
        response (http/post (wminer-urls wminer-url-key) request)
        wiki-doc (:body response)
        skip-single (not (false? (:skip-single opt)))
        get-title (fn [wiki-link]
                    (let [[page-title anchor-text] (string/split wiki-link #"\|")
                          anchor-text (or anchor-text wiki-link)
                          page-title (or page-title wiki-link)]
                      (when (or (.contains anchor-text " ")
                                (not skip-single))
                        page-title)))
        pages (fn [annotated-snippet]
                (let [links (map second (re-seq #"\[\[(.*?)\]\]" annotated-snippet))]
                  (set (map string/lower-case (keep get-title links)))))]
    (map pages (string/split wiki-doc (re-pattern delim)))))
