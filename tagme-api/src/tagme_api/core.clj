(ns tagme-api.core
  (:require [utils.core :as u]
            [clojure [string :as string]]
            [clj-http.client :as http]))

(def ^:private TAGME_URL "http://tagme.di.unipi.it/")

(def ^:private APP_KEY "ghtma192")

(def default-params {})

(defn tagme-annotate-one [snippet]
  (let [request {:form-params {:text snippet
                               :key APP_KEY}
                 :as :json}
        update-fn #(-> %
                     (update-in [:title] string/lower-case)
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
