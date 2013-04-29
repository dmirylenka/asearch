(ns wikidb.core
  "Main package for dealing with wikipedia data."
  (:require [clojure [string :as string]]
            [clojure.java [jdbc :as sql]]
            [utils.core :as u]))

(defn wiki-normalize
  "Turns 'computer science' into 'Computer_science'"
  [category-name]
  (-> category-name
    (string/trim)
    string/lower-case
    (string/replace #"\s+" "_")
    string/capitalize))

(defn wiki-unnormalize
  "Turns 'Computer_science' into 'computer science'"
  [category-name]
  (-> category-name 
    (string/trim)
    string/lower-case
    (string/replace #"_" " ")
    (string/replace #"\s+" " ")))

(def ^:private byte-array-class (Class/forName "[B"))

(defn byte-array? [obj]
  (instance? byte-array-class obj))

(defn stringify [obj]
  (if (byte-array? obj) (String. obj) obj))

(defn cure-strings [obj]
  (u/map-val stringify obj))

(defn select
  "Performs an sql select query on a given db.
     Example: (select db \"* from users\")."
  [query & params]
  (let [coll-params (filter coll? params)
        q-marks #(str "(" (string/join "," (repeat (count %) "?")) ")")
        replace-coll-params (fn replace-coll-params [query [first-param & rest-params]]
                             (if-not first-param query
                               (recur (string/replace-first query "??" (q-marks first-param))
                                      rest-params)))
        query-and-params (vec (cons (str "select " (replace-coll-params query coll-params)) (flatten params)))]
    (when-not (some empty? coll-params)
      (map cure-strings
           (sql/with-query-results results query-and-params (doall results))))))

(defn page-categories
  "Returns the list of parent categories for the page."
  [page-title]
  (let [results (select "cl_to as result 
                           from page join categorylinks
                           on page_id = cl_from
                           where page_title = ?
                           and cl_type='page'
                           and page_namespace=0"
                        page-title)]
    (map #(String. (:result %)) results)))

(defn super-categories
  "Returns the list of parent categories for the category."
  [category]
  (let [results (select "cl_to as result 
                           from page join categorylinks
                           on page_id = cl_from
                           where page_title = ?
                           and cl_type='subcat'
                           and page_namespace=14"
                        category)]
    (map #(String. (:result %)) results)))

(defn cat-relations
  "Returns the list of child-parent relations between given categories."
  [topics]
  (let [results (select "page_title, cl_to 
                           from page join categorylinks
                           on page_id = cl_from
                           where page_title in ?? 
                           and cl_to in ??
                           and cl_type='subcat'
                           and page_namespace=14"
                        topics, topics)]
    (map (comp (partial map #(String. %)) vals) results)))

(defn category-pages
  "Returns the list of pages belonging (directly) to the category."
  [category]
  (let [results (select "page_title as result 
                           from page join categorylinks
                           on page_id = cl_from
                           where cl_to = ?
                           and cl_type='page'
                           and page_namespace = 0"
                       category)]
    (map #(String. (:result %)) results)))

(defn sub-categories 
  "Returns the list of the subcategories for a category."
  [category]
  (let [results (select "page_title as result 
                           from page join categorylinks
                           on page_id = cl_from
                           where cl_to = ?
                           and cl_type='subcat'
                           and page_namespace = 14"
                       category)]
    (map #(String. (:result %)) results)))

(defn redirect-from
  "When the page is a redirect, returns the the page it ponts to.
   Otherwise returns nil." 
  [redirect-page-id]
  (let [results (select "* from page
                        where page_namespace = 0
                        and page_title = (
                          select rd_title
                          from redirect
                          where rd_from = ?
                          and rd_namespace=0)"
                        redirect-page-id)]
    (first results)))

(defn page-by-title
  "Returns the data about wikipedia page by its title."
  [page-title]
  (let [results (select "* from page
                           where page_title = ?
                           and page_namespace = 0"
                        page-title)]
    (first results)))

(def hidden-categories
  "The set of hidden categories in Wikipedia."
  (delay (set (sub-categories "Hidden_categories"))))

(defmacro declare-remove-hidden
  "For declaring functions of page and category titles that filter out
   hidden categories from the result list."
  [func-name func]
  `(defn ~func-name [title# & more#]
     (let [opt# (apply hash-map more#)
          hidden# (opt# :hidden)
          all# (~func title#)]
      (if hidden# all#
        (remove @hidden-categories all#)))))

(declare-remove-hidden cats page-categories)
(declare-remove-hidden subcats sub-categories)
(declare-remove-hidden supercats super-categories)

(defn supercat-rels
  "Returns the list of child-parent relations between given categories."
  [topics]
  (let [results (select "page_title, cl_to 
                           from page join categorylinks
                           on page_id = cl_from
                           where page_title in ?? 
                           and cl_type='subcat'
                           and page_namespace=14"
                        topics)]
    (->> results
      (map (comp (partial map #(String. %)) vals))
      (remove (comp @hidden-categories second))
      (remove (comp #(re-find #"^\d+$" %) second))
      (remove (comp #(re-find #"_by_" %) second)))))

(defn page-cats
  "Returns Wikipedia categories to which the page belongs. Ignores disambiguation pages."
  [page & more]
  (let [opt (apply hash-map more)
        skip-ambiguous (get opt :skip-ambiguous true)
        categories (cats page)
        is-ambiguous (some #{"Disambiguation_pages"} categories) 
        should-skip (and is-ambiguous skip-ambiguous)]
    (when-not should-skip categories)))
