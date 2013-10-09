(ns acm-map-exp.experiment
  (:use [clojure.pprint :only [print-table pprint]])
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [acm-map-exp.util :as u]
            [wiki-api.core :as wapi])
  (:import [java.util Date]
           [java.text SimpleDateFormat]))

(defn- time-str [conf]
  (str "-" (.format (SimpleDateFormat. (:date-format conf)) (:date-time conf))))

(defn- ext-str [conf]
  (str "." (:file-ext conf)))

(defn- suffix-str [conf]
  (or (:suffix conf) ""))

(def file-name-fns
  {:type-only
   (fn type-only [data data-type conf]
     (str data-type (ext-str conf)))
   :type-time
   (fn type-time [data data-type conf]
     (str data-type (time-str conf) (ext-str conf)))
   :type-suffix
   (fn type-suffix [data data-type conf]
     (str data-type (suffix-str conf) (ext-str conf)))
   :type-suffix-time
   (fn custom-suffix [data data-type conf]
     (str data-type (suffix-str conf) (time-str conf) (ext-str conf)))})

(def default-conf {:data-dir "./resources/data/"
                   :exp-dir "./exp/"
                   :exp-name nil
                   :file-ext "clj"
                   :date-time nil
                   :suffix nil
                   :date-format "YY-MM-dd'T'HH:mm:ss"
                   :file-name-fn :type-suffix-time ; (fn [data data-type conf])
}) 

(defn new-conf* [opt]
  (-> default-conf
      (merge opt)
      (assoc :date-time (Date.))))

(defn new-conf [& {:as opt}]
  (new-conf* opt))

;;;; General experiment functionality (running, saving, reporting, etc.) ;;;;

(defn- file-path [data data-type conf]
  {:pre [(not (string/blank? (:exp-name conf)))]}
  (let [{:keys [data-dir exp-dir file-name-fn exp-name]} conf
        file-name-fn (or (file-name-fns file-name-fn) file-name-fn)] ; file-name-fn was either a function or a keyword
    (str data-dir "/" exp-dir "/" exp-name "/" (file-name-fn data data-type conf))))

(defn save-to-file
  ([data file-name]
     (let [file (io/file file-name)]
       (io/make-parents file)
       (with-open [w (io/writer file)]
         (pprint data w))
       data))
  ([data data-type conf]
     (save-to-file data (file-path data data-type conf))))

;; String -> Any
(defn read-from-file
  "Reads clojure data from the file."
  ([file-name]
     (read-string (slurp file-name))))

;; ::report ::conf -> nil
(defn display-report
  [report conf]
  (doseq [[name {:keys [descr data]}] report]
    (print descr)
    (print-table data)
    (println)))

(defn run-experiment
  [exp input-data conf]
  (let [{:keys [generate-data compute-stats generate-report]} exp]
    (save-to-file conf "config" conf)
    (-> (generate-data input-data conf)
        (save-to-file "raw-data" conf)
        (compute-stats conf)
        (save-to-file "stats" conf)
        (generate-report conf)
        (save-to-file "report" conf)
        (display-report conf))))
