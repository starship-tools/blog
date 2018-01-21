(ns starship.blog.email.content
  (:require
    [clojure.java.io :as io]
    [clojusc.twig :refer [pprint]]
    [dragon.blog.core :as blog]
    [dragon.selmer.core :refer [render]]
    [starship.blog.web.content.data :as data]
    [taoensso.timbre :as log]
    [trifl.fs :as fs]))

(def new-post-file "new-post.html")

(defn get-filename
  [post-data file-name]
  (format "%s/%s" (:src-dir post-data) file-name))

(defn get-new-post-filename
  [post-data]
  (get-filename post-data new-post-file))

(defn get-new-post-email-content
  [system post-data]
  (render
    (str "templates/emails/" new-post-file)
    (data/post system [] post-data)))

(defn gen-new-post-email
  [system post-data]
  (let [file-data (get-new-post-email-content system post-data)
        outfile (get-new-post-filename post-data)]
    (when-not (fs/file-exists? (io/file outfile))
      (log/debug "Generating 'new post' email content for" post-data)
      (spit
        outfile
        (get-new-post-email-content system post-data)))))

(defn gen
  [system posts]
  (log/debug "Generating emails ...")
  (log/trace "Got data:" (pprint (blog/data-for-logs posts)))
  (doseq [post-data posts]
    (gen-new-post-email system post-data))
  :ok)
