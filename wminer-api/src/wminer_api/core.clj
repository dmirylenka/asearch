(ns wminer-api.core
  (:require [utils.core :as u]
            [wiki-api.core :as wapi]
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
   :linkFormat "html_id_weight"
   :responseFormat "direct"})

(def param-mapping
  {:min-prob :minProbability
   :repeat-mode :repeatMode
   :link-format :linkFormat
   :response-format :responseFormat
   :source :source})

;(defn wiki-articles [snippets & more]
;  (let [opt (apply hash-map more)
;        delim " #@ "
;        query (string/join delim snippets)
;        custom-params (u/map-key param-mapping (select-keys opt (keys param-mapping)))
;        request {:form-params (-> default-params
;                                (merge custom-params)
;                                (assoc :source query))}
;        wminer-url-key (or (:wminer opt) :lion)
;        response (http/post (str (wminer-urls wminer-url-key) "wikify") request)
;        wiki-doc (:body response)
;        skip-single (not (false? (:skip-single opt)))
;        get-title (fn [wiki-link]
;                    (let [[page-title weight anchor-text] (string/split wiki-link #"\|")
;                          anchor-text (or anchor-text wiki-link)
;                          page-title (or page-title wiki-link)]
;                      (when (or (.contains anchor-text " ")
;                                (not skip-single))
;                        page-title)))
;        pages (fn [annotated-snippet]
;                (let [links (map second (re-seq #"\[\[(.*?)\]\]" annotated-snippet))]
;                  (set (map string/lower-case (keep get-title links)))))]
;    (map pages (string/split wiki-doc (re-pattern delim)))))

(def ^:private link-pattern #"<a href=\"http\://www\.en\.wikipedia\.org/wiki/(.*?)\".*? pageId=\"(.*?)\".*? linkProb=\"(.*?)\".*?>(.*?)</a>")

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
        pages (fn [annotated-snippet]
                (for [[_ page-title page-id link-prob fragment] (re-seq link-pattern annotated-snippet)
                      :when (.contains fragment " ")] 
                  [(string/lower-case page-title)
                   (Integer/parseInt page-id)
                   (Double/parseDouble link-prob)
                   fragment]))]
    (map pages (string/split wiki-doc (re-pattern delim)))))

;(defn wiki-similarity* [term1 term2 & {:as opt}]
;  (let [request {:form-params {:term1 term1 :term2 term2}}
;        wminer-url-key (or (:wminer opt) :lion)
;        response (http/post (str (wminer-urls wminer-url-key) "compare") request)
;        xml (xml/parse (java.io.StringReader. (:body response)))]
;    (->> xml
;      :content
;      (filter #(= (:tag %) :Response)) first
;      :attrs :relatedness Double/parseDouble)))

;(defn wiki-similarity [term-pairs]
;  (for [[term1 term2] term-pairs]
;    [[term1 term2] (wiki-similarity* term1 term2)]))


(defn wiki-similarity [id1 id2 & {:as opt}]
  (let [request {:form-params {:id1 id1 :id2 id2}}
        wminer-url-key (or (:wminer opt) :lion)
        response (doto (http/post (str (wminer-urls wminer-url-key) "compare") request) println)
        xml (xml/parse (java.io.StringReader. (:body response)))]
    (->> xml
      :content
      (filter #(= (:tag %) :Response)) first
      :attrs :relatedness Double/parseDouble)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; new stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype WikiService []
  wapi/IWikiService
  (-annotate [this docs]
    (let [doc-strings (map wapi/doc-string docs)
          wiki-pages (wiki-articles doc-strings)]
      (for [[doc pages] (map vector docs wiki-pages)
            [title id prob fragment] pages]
        (wapi/->DocArticleLink doc (wapi/->Article id title) fragment prob))))
  (-relatedness [this article-pairs]
    (for [[a1 a2] article-pairs
          :let [score (wiki-similarity (wapi/article-id a1) (wapi/article-id a2))]]
      (wapi/->ArticleRel #{a1 a2} score)))
  (-article-categories [this article] nil)
  (-cat-relations [this categories] nil))

(def service (WikiService.))
