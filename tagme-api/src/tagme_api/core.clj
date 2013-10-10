(ns tagme-api.core
  (:require [utils.core :as u]
            [clojure [string :as string]]
            [clj-http.client :as http]
            [wiki-api [core :as api]]))

(def ^:private TAGME_URL "http://tagme.di.unipi.it/")

;; TODO: move to the configuration
(def ^:private APP_KEY "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; old stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tagme-annotate-one [snippet]
  (let [request {:form-params {:text snippet
                               :key APP_KEY}
                 :as :json}
        update-fn #(-> %
                     ;(update-in [:title] string/lower-case)
                     (update-in [:rho] (fn [s] (Double/parseDouble s))))
        response (http/post (str TAGME_URL "tag") request)]
    (map update-fn (:annotations (:body response)))))

(defn tagme-articles [snippets]
  (map tagme-annotate-one snippets))

(defn relatedness* [topic-id-pairs]
  (let [request {:form-params {:id (mapv (partial string/join " ") topic-id-pairs)
                               :key APP_KEY}
                 :as :json}
        http-response (http/post (str TAGME_URL "rel") request)
        response (:body http-response)
        _ (when-not (zero? (:errors response)) (throw (Exception. (str "Exception while computing relatedness between topics " topic-id-pairs))))]
    (for [{:keys [couple rel]} (:result response)
           :let [rel (Double/parseDouble rel)]
           :when (> rel 0)]
      [(map #(Integer/parseInt %) (string/split couple #"\s"))
       rel])))

(defn wiki-articles [snippets]
  (map (partial map :title) (tagme-articles snippets)))

(defn relatedness [topic-id-pairs]
  (let [pair-chunks (partition-all 100 topic-id-pairs)]
    (apply concat (map relatedness* pair-chunks))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; new stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (deftype WikiService []
;;   api/IWikiService
;;   (-annotate [this docs]
;;     (for [doc docs
;;           annotation (tagme-annotate-one (api/doc-string doc))
;;           :when (.contains (:spot annotation) " ")
;;           ]
;;       (api/->DocArticleLink doc
;;                             (api/->Article (:id annotation) (:title annotation))
;;                             (:spot annotation)
;;                             (:rho annotation))))
;;   (-relatedness [this article-pairs]
;;     (let [articles (distinct (apply concat article-pairs))
;;           article-ids (map api/article-id articles)
;;           article-map (u/key-map api/article-id articles) 
;;           id-pairs (for [id1 article-ids
;;                          id2 article-ids
;;                          :when (> id2 id1)]
;;                      [id1 id2])
;;           rel-scores (relatedness id-pairs)]
;;           (for [[[id1 id2] score] rel-scores]
;;             (api/->ArticleRel #{(article-map id1) (article-map id2)} score))))
;;   (-article-categories [this article] nil)
;;   (-cat-relations [this categories] nil))

;; (def service (WikiService.))
