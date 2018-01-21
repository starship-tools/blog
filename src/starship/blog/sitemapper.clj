(ns starship.blog.sitemapper
  (:require
    [clojure.data.xml :as xml]
    [clojusc.twig :refer [pprint]]
    [dragon.config.core :as config]
    [dragon.util :as util]
    [taoensso.timbre :as log]
    [trifl.xml :as xml-util]))

(defn url
  [datestamp route]
  (log/debug "Generating sitemap entry for" route)
  [:url
   [:loc (str "http://starship.tools" route)]
   [:lastmod datestamp]
   [:changefreq "weekly"]])

(defn urlset
  [datestamp routes]
  [:urlset {:xmlns "http://www.sitemaps.org/schemas/sitemap/0.9"}
   (map (partial url datestamp) (keys routes))])

(defn gen
  [system routes]
  (let [disallowed (config/robots-disallow system)
        allowed-routes (sort (util/remove-routes disallowed routes))
        datestamp (util/format-datestamp (util/now :datetime-map))]
    (log/debug "Disallowed prefixes:" disallowed)
    (log/trace "Allowed routes:" (keys allowed-routes))
    (xml-util/pretty-xml
     (xml/emit-str
      (xml/sexp-as-element
       (urlset datestamp allowed-routes))))))
