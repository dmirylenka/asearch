(ns wikidb.config)

(def wiki-db
  "Wikipedia database configuration."
  {:classname "com.mysql.jdbc.Driver"
   :subprotocol "mysql"
   :subname (str "//" "127.0.0.1" ":" 33066 "/" "wiki")
   :user "dmirylenka"
   :password "dmirylenka"})
