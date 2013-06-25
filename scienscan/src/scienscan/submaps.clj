(ns scienscan.submaps
  (:require
   [clojure.string :as string]
   [compojure.handler :as handler]
   [compojure.route :as route]
   [cheshire [core :as json]]
   [utils.core :as u]
   [mas-api.core :as mas]
   [topic-maps.core :as tmaps]))

(defn build-topic-map [s-results]
  (let [time (System/currentTimeMillis)]
  (-> s-results
   (doto (#(println (count %) "results")))
   tmaps/init-topic-map
   tmaps/link-to-articles
   (doto (#(println (count (tmaps/get-topics %)) "articles in" (/ (- (System/currentTimeMillis) time) 1000.0))))
   tmaps/retrieve-categories
   (doto (#(println (count (tmaps/get-topics %)) "articles and categories")))
   tmaps/link-categories
   (doto ((fn [_] (println "categories linked"))))
   tmaps/merge-similar
   (doto (#(println (count (tmaps/get-topics %)) "articles and categories after merge")))
   tmaps/break-loops
   (doto ((fn [_] (println "cycles broken")))))))

(defn best-by-greedy-coverage [topic-map n-topics]
  (let [all-topics (tmaps/get-topics topic-map)
        iter  (fn [topics covered-docs]
                (if (= n-topics (count topics))
                  topics
                  (let [candidate-topics (remove (set topics) all-topics)
                        docs-covered-with (u/val-map
                                           #(into covered-docs
                                                  (tmaps/proper-docs topic-map %))
                                           candidate-topics)
                        score-fn (comp count docs-covered-with)
                        next-topic (apply max-key score-fn candidate-topics)]
                    (recur (conj topics next-topic)
                           (docs-covered-with next-topic)))))
        topics (iter [] #{})]
    (tmaps/submap topic-map topics)))
