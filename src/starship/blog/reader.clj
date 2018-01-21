(ns starship.blog.reader
  (:require
    [clojure.data.xml :as xml]
    [clojusc.twig :refer [pprint]]
    [dragon.config.core :as config]
    [taoensso.timbre :as log]
    [trifl.xml :as xml-util]))

(defn atom-entry
  [system post]
  (let [uri-posts (config/posts-path system)
        uri (str uri-posts (:uri-path post))]
    [:entry
     [:title (:title post)]
     [:updated (:timestamp post)]
     [:author [:name (:author post)]]
     [:link {:href (format "http://%s/%s" (config/domain system) uri)}]
     [:id (format "%s:feed:post:%s" (config/domain-urn system) (:title post))]
     [:content {:type "html"} (:body post)]]))

(defn atom-feed
  [system route posts]
  (xml-util/pretty-xml
   (xml/emit-str
    (xml/sexp-as-element
     [:feed {:xmlns "http://www.w3.org/2005/Atom"}
      [:id (format "%s:feed" (config/domain-urn system))]
      [:updated (-> posts first :timestamp)]
      [:title {:type "text"} (config/name system)]
      [:link {:rel "self" :href (format "http://%s%s" (config/domain system) route)}]
      (map (partial atom-entry system) posts)]))))
