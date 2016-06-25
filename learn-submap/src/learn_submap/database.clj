(ns learn-submap.database
  (:require [clojure.java.io :as io]
            [clojure.string :refer [split blank?]]
            [clojure.java.jdbc :as jdbc]
            [clojure.java.jdbc.sql :as sql]
            [learn-submap.lrn-sbm :as ls]
            [topic-maps.core :as tmaps]
            [utils.core :as u])
  (:import [learn_submap.lrn_sbm TopicSubmap]))

(def default-db
  "An example of the DB configuration."
  {:subprotocol "mysql"
   :subname "//localhost:3306/msdataset"
   :user "dmirylenka"
   :password "dmirylenka"})

(defn -save-ground-truth!
  "Saves the piece of 'ground truth' topic summary data into the database.
   The data consists of an initial state - a topic map of type learn-submap.lrn.sbm.TopicSubmap,
   and a sequence of optimal actions - topics that comprise the optimal summaries of the topic map."
  [db init-state action-seq]
  (jdbc/insert! db :ground_truth
    {:init_state (pr-str init-state) :action_seq (pr-str action-seq)})
  nil)

(defn -compute-features [features init-state]
  (let [topic-map (:topic-map init-state)
        feature-vals (mapv #(%1 topic-map) features)]
    (assoc init-state
      :features features
      :feature-vals feature-vals)))

(defn -read-ground-truth
  "Reads the 'ground truth' topic summaries from the database (see save-ground-truth)."
  [features db]
  (let [results (jdbc/query db
                  (sql/select *
                    :ground_truth))]
    (map (juxt (comp (partial -compute-features features) read-string :init_state)
               (comp read-string :action_seq)) results)))

(defn read-ground-truth [features & {:as db}]
  (-read-ground-truth features (merge default-db db)))

(defn save-ground-truth! [init-state action-seq & {:as db}]
  (-save-ground-truth! (merge default-db db) init-state action-seq))

(defn fix-merged []
  (let [fixed-ground-truth
          (for [[init-state action-seq] (read-ground-truth nil)]
            [(update-in init-state [:topic-map] tmaps/cache-merged)
             action-seq])]
    (doseq [[init-state action-seq] fixed-ground-truth]
      (save-ground-truth! init-state action-seq))))

(defn fix-main []
  (let [fixed-ground-truth
          (for [[init-state action-seq] (read-ground-truth nil)]
            [(update-in init-state [:topic-map] u/assocf tmaps/main-topic identity :main-topic)
             action-seq])]
    (doseq [[init-state action-seq] fixed-ground-truth]
      (println (:main-topic (:topic-map init-state)))
      (save-ground-truth! init-state action-seq))))
