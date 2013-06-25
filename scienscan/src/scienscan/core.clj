(ns scienscan.core
  (:require [topic-maps.core :as tm]
            [clojure.string :as string]))

(defrecord Paper [title abstract]
  tm/IDocument
    (doc-id [this]
      (str (:title this) " "
           (:year this) " "
           (doall (string/join ", " (map :last-name (:author this))))))
    (doc-string [this]
      (str (:title this) ". " (:abstract this))))




