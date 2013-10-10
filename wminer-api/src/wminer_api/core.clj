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
  {:prob :minProbability
   :repeat-mode :repeatMode
   :link-format :linkFormat
   :response-format :responseFormat
   :source :source})

(def ^:private link-pattern #"<a href=\"http\://www\.en\.wikipedia\.org/wiki/(.*?)\".*? pageId=\"(.*?)\".*? linkProb=\"(.*?)\".*?>(.*?)</a>")

(defn wiki-articles [snippets & more]
  (let [opt (apply hash-map more)
        delim " #@ "
        query (string/join delim snippets)
        custom-params (u/map-key param-mapping (select-keys opt (keys param-mapping)))
        request {:form-params (-> default-params
                                (merge custom-params)
                                (assoc :source query))}
        wminer-url-key (or (:wminer opt) :waikato)
        response (http/post (str (wminer-urls wminer-url-key) "wikify") request)
        wiki-doc (:body response)
        skip-single (not (false? (:skip-single opt)))
        pages (fn [annotated-snippet]
                (for [[_ page-title page-id link-prob fragment] (re-seq link-pattern annotated-snippet)
                      ;; :when (.contains fragment " ")
                      ] 
                  [page-title
                   (Integer/parseInt page-id)
                   (Double/parseDouble link-prob)
                   fragment]))]
    (map pages (string/split wiki-doc (re-pattern delim)))))

(defn wiki-similarity [id1 id2 & {:as opt}]
  (let [request {:form-params {:id1 id1 :id2 id2}}
        wminer-url-key (or (:wminer opt) :lion)
        response (doto (http/post (str (wminer-urls wminer-url-key) "compare") request) println)
        xml (xml/parse (java.io.StringReader. (:body response)))]
    (->> xml
      :content
      (filter #(= (:tag %) :Response)) first
      :attrs :relatedness Double/parseDouble)))

(defn ->wapi-category [cat]
  (wapi/mk-category (:id cat) (:title cat)))

(def explore-services
  {:wiki-api.core/article "exploreArticle"
   :wiki-api.core/category "exploreCategory"})

(defn parent-cats [page & {:as opt}]
  (let [request {:form-params {:id (:id page)
                               :responseFormat "json"
                               :parentCategories true}
                 :as :json}
        wminer-url-key (or (:wminer opt) :waikato)
        page-type (or (:type page) :wiki-api.core/article)
        service-name (explore-services page-type)
        response (http/post (str (wminer-urls wminer-url-key) service-name) request)
        response-article (:body response)
        cats (:parentCategories response-article)]
    (map ->wapi-category cats)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; new stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype WikiService []
  wapi/IWikiService
  (-annotate [this strings]
    (wapi/-annotate this strings 0.1))
  (-annotate [this strings prob]
    (let [wiki-pages (wiki-articles strings :prob prob)
          mk-link (fn [[title id prob fragment]]
                       (wapi/->ArticleLink (wapi/mk-article id title) fragment prob))]
      (map #(map mk-link %) wiki-pages)))
  ;; (-relatedness [this article-pairs]
  ;;   (for [[a1 a2] article-pairs
  ;;         :let [score (wiki-similarity (wapi/article-id a1) (wapi/article-id a2))]]
  ;;     (wapi/->ArticleRel #{a1 a2} score)))
  (-article-categories [this article]
    (parent-cats article))
  (-cat-relations [this cats]
    (let [cats (set cats)]
      (for [cat cats
            parent (parent-cats cat)
            :when (cats parent)]
        [cat parent])))
)

(def service (WikiService.))
