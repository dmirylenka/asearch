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
   :subname "//localhost:3306/topic_sum"
   :user "root"
   :password ""})

(defn -save-ground-truth!
  "Saves the piece of 'ground truth' topic summary data into the database.
   The data consists of an initial state - a topic map of type learn-submap.lrn.sbm.TopicSubmap,
   and a sequence of optimal actions - topics that comprise the optimal summaries of the topic map."
  [db init-state action-seq]
  (jdbc/insert! db :ground_truth
    {:init_state (pr-str init-state) :action_seq (pr-str action-seq)})
  nil)

(defn -read-ground-truth
  "Reads the 'ground truth' topic summaries from the database (see save-ground-truth)."
  [db]
  (let [results (jdbc/query db
                  (sql/select *
                    :ground_truth))]
    (map (juxt (comp read-string :init_state)
               (comp read-string :action_seq)) results)))

(defn read-ground-truth [& {:as db}]
  (-read-ground-truth (merge default-db db)))

(defn save-ground-truth! [init-state action-seq & {:as db}]
  (-save-ground-truth! (merge default-db db) init-state action-seq))
