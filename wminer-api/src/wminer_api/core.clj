(ns wminer-api.core
  (:require [utils.core :as u]
            [clojure [string :as string]]
            [clj-http.client :as http]
            [clojure.data [xml :as xml]]))

(def ^:private 
  wminer-urls {
    :waikato "http://wikipedia-miner.cms.waikato.ac.nz/services/"
    :lion "http://lion0b.disi.unitn.it:8082/wikipedia-miner/services/"})

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
        response (http/post (str (wminer-urls wminer-url-key) "wikify") request)
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

(defn wiki-similarity* [term1 term2 & {:as opt}]
  (let [request {:form-params {:term1 term1 :term2 term2}}
        wminer-url-key (or (:wminer opt) :lion)
        response (http/post (str (wminer-urls wminer-url-key) "compare") request)
        xml (xml/parse (java.io.StringReader. (:body response)))]
    (->> xml
      :content
      (filter #(= (:tag %) :Response)) first
      :attrs :relatedness Double/parseDouble)))

(defn wiki-similarity [term-pairs]
  (for [[term1 term2] term-pairs]
    [[term1 term2] (wiki-similarity* term1 term2)]))
