(ns utils.text
  (:use utils.core)
  (:require (clojure [string :as string]
                     [set :as set])
            (clojure.java [io :as io])))

(def stop-words
  "The list of english stop words."
  (delay (set (line-seq (io/reader (io/file "./resources/stoplist.txt"))))))

(defn string->words
  "Turns a string of text into a sequence of lowercase words.
   Splitting is not lazy, lowercasing is."
  [s]
#_  {:pre [(string? s)]}
  (let [result (->> (string/split s #"[^\p{L}\d-]+")
		(map string/lower-case)
		(filter #(re-matches #"^\p{L}.*$" %)))]
    (when (empty? result)
      (println (str "Wordless string: " s)))
    result))

(defn stem-string
  "Stems the string using Porter Stemmer."
  [s]
  {:pre [(string? s)
         (not (string/blank? s))]}
  (-> (doto
        (org.tartarus.martin.Stemmer.)
        (.add (char-array s) (count s))
        .stem)
      .toString)) 

(defn stem
  "Stems the sequence of words using Porter Stemmer."
  [words]
  {:pre [(seq words)]}
  (map stem-string words))

(defn remove-stop-words
  "Removes stop words from the sequence."
  [words]
 #_ {:pre [(seq words)
         (every? string? words)]}
  (let [result (remove @stop-words words)]
    (when (empty? result)
      (println (str "Stopword sequence: " (string/join " " words))))
    result))

(defn words->bigrams
  "Transforms the collection of words into a collection of bigrams."
  [words]
  (map str words (repeat " ") (rest words)))

(defn words->counts
  "Transforms the collection of words into a map from words
   to the number of times they appear in the collection."
  [words]
 #_ {:pre [(seq words)
         (every? string? words)]}
  (frequencies words))

(defn words->1
  "Transforms the collection of words into a map,
   in which keys are distinct words and all values equal to 1."
  [words]
  {:pre [(seq words)
         (every? string? words)]}
  (into {} (map #(vector % 1) words)))
  

(defn remove-rare
  "Given a collection of documents represented by term counts (see words->counts),
   removes words with lower than thresold number of occurrences in the collection."
  [threshold docs]
 #_ {:pre [(seq docs)
         (every? map? docs)
         (every? not-empty docs)]}
  (let [total-docs (apply merge-with + docs)
        filter-fn #(< (second %) threshold)
        rare-words (->> total-docs
                     (filter filter-fn)
                     (map first))]
    (map #(apply dissoc % rare-words) docs)))

(defn remove-rare-seq
  "Given a collection of documents represented by term sequences
   removes words with lower than thresold number of occurrences in the collection."
  [threshold docs]
 #_ {:pre [(seq docs)
         (every? seq docs)]}
  (let [counts (map words->counts docs)
        total-counts (apply merge-with + counts)
        filter-fn #(< (second %) threshold)
        rare-words (->> total-counts
                     (filter filter-fn)
                     (map first)
                     set)]
    (map #(remove rare-words %) docs)))

(defn counts->freqs
  "Transfoms the term counts (see words->counts) into their relative frequencies
  (with respect to the most frequent term in the document)."
  [counts]
  {:pre [(map? counts)
        #_ (not-empty counts)]}
  (if-not (empty? counts)
    (let [max-count (apply max (vals counts))
          norm #(/ % max-count)]
      (map-val norm counts))
    counts))

