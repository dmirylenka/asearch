(ns scienscan.core
  (:require [topic-maps.core :as tm]
            [wiki-api.core :as wapi]
            [clojure.string :as string]))

(defrecord Paper [title abstract]
  tm/IDocument
  (doc-id [this]
    (str (:title this) " "
         (:year this) " "
         (doall (string/join ", " (map :last-name (:author this))))))
  wapi/IDocument
  (doc-string [this]
    (str (:title this) ". " (:abstract this))))
