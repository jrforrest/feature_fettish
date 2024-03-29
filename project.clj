(defproject feat_fettish "0.0.1-INITIAL"
  :description "Feature identification for Reddit articles"
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [clj-http "0.5.5"]
                 [enlive "1.1.1"]
                 [io.curtis/boilerpipe-clj "0.2.0"]
                 [org.clojure/tools.logging "0.2.6"]
                 [com.taoensso/carmine "2.0.0-RC1"]
                 [http-kit "2.1.5"]
                 [org.pld/clatrix "0.3.0-SNAPSHOT"]
                 [hiccup "1.0.4"]
                 [org.clojure/data.json "0.2.2"]
                 [compojure "1.1.5"]]
  :plugins [[lein-marginalia "0.7.1"]])
