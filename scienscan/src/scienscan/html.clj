(ns scienscan.html
  (:use [hiccup.def :only [defhtml]]
        [hiccup.form :only [text-field submit-button form-to]]
        [hiccup.element :only [javascript-tag image]]
        [hiccup.page :only [include-css include-js html5]])
  (:require [cheshire [core :as json]]
            [utils.core :as u]))

(def logo 
  [:div#logo
   [:div#logo-scien "Scien"]
   [:div#logo-scan "Scan"]
   [:div#subtitle "Explore the structure of scientific topics!"]
  [:div#menu
    [:a {:class "menu-link" :href "/"} "home"] "|" 
  #_ [:a {:class "menu-link" :href "about"} "about"]]])

(def footer-content
  [:div#footer-content {:class "footer-content"}
   [:div#copyright {:class "copyright"} "© 2012-2013 University of "
    [:a {:href "http://disi.unitn.it/"} "Trento"]]
   [:div#contacts {:class "contacts"}
    [:address "Contact: " [:a {:href "http://disi.unitn.it/users/daniil.mirylenka"} "Daniil Mirylenka"]]]])

(defhtml layout [& content]
  [:head
   [:title "ScienScan"]]
  (include-css "/css/mas.css")
  (include-css "/css/asearch.css")
  (include-css "/css/ui-lightness/jquery-ui-1.10.0.custom.css")
  (include-css "/css/ui-lightness/jquery-ui-1.10.0.custom.css")
  (include-js "/js/jquery-1.9.0.js")
  (include-js "/js/jquery-ui-1.10.0.custom.js")
  [:body
   [:div#whole-thing
    [:header#header logo]
    [:div#left "&nbsp;"]
    [:div#center content]
    [:div#right "&nbsp;"]
    [:footer#footer footer-content]]])

(def mas [:a {:href "http://academic.research.microsoft.com/"} "Microsoft Academic Search"])

(def try-again "Try again later.")

(def error-messages
  {:forbidden [mas " rejected our request. Probably we exceeded the quota of 200 requests per minute. " try-again]
   :bad-request ["the request we sent to " mas " was invalid. It may be a bug in our or their service."] 
   :unavailable [mas " is temporary unavailable." try-again]
   :empty-results [mas " returned no results for your query. Either you misspelled it, or " mas " is down. Check your query or try again later."]
   :timeout [mas " did not respond within " 30 " seconds. " try-again]})

(defn sorry [error-code]
  (vec (concat [:div#sorry "Sorry, "]
          (or (error-messages error-code)
              ["something unexpected happened: " error-code]))))

(defn search-field [query]
  [:input#search-box {:class "search-box" :type "text" :name "query" :value query}])

(def search-button
  [:input#search-button {:type "image" :class "search-button" :src "/Images/search_button_wide.png"}])

(defn search-controls [{:keys [query]}]
  [:div#search-controls {:class "search-controls"}
    (form-to [:get "/"]
      [:div#search-bar {:class "search-bar"}
       [:div#type-invitation "Type your query and see the results:"]
        (search-field query)
        search-button])])

(defn slider [n-topics-min n-topics-max n-topics]
 (let [n-topics-min (or n-topics-min 1)
       n-topics-max (min n-topics-max 25)]
   [:div#minFreqSlider
    (javascript-tag (str "$(function() {
                          $('#minFreqSlider').slider({
                            min: " n-topics-min ",
                            max: " n-topics-max ",
                            orientation: 'horisontal',
                            value: " n-topics ",
                              step: 1,
                            slide: function(event, ui) {
                                ScienScanJs.refine(ui.value);
                                //$('#minFreqValue').val(ui.value);
                              }
                            });
                            $('#minFreqValue').val($('#minFreqSlider').slider('value'));
                         });"))]))

(defn topic-controls [data]
  (let [{:keys [n-topics n-topics-max svg]} data]
    [:div#topic-controls {:class "topic-controls"}
     [:div#graph-title {:class "graph-title"}]
     [:div#slider-title 
      [:div "Move the slider to tune the topic granularity:"]
      [:div "&darr;"]]
       [:div#slider-bar
        [:div#show-less "Less &rarr;"] 
        (slider nil n-topics-max n-topics)
        [:div#show-more "&larr; More"]]
     [:div#topic-graph {:class "topic-graph"} (:value svg)]
     [:div#click-invitation 
      [:div "&uarr;"#_"&#X2197;"]
      [:div "Click on the topics to filter the results "]]]))

(defn search-results [data]
  (let [topic-vis-data (:value (:json data))]
    [(javascript-tag (str "var topicVisData = " (json/encode topic-vis-data) "; "))
     (include-js "/js/asearch.js")
     [:div#result-section {:class "result-section"}
      [:span#result-title {:class "result-title"}]
      [:div#result-list {:class "result-list"}]]]))

(defn search-page-html [data]
  (let [{:keys [query results svg]} data]
    (cond-> [(search-controls data)]
            (u/success? svg) (concat [(topic-controls data)])
            (u/success? results) (concat (search-results data))
            (u/fail? svg) (concat [(sorry (:error svg))]))))
