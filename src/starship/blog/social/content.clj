(ns starship.blog.social.content
  (:require
    [clojure.java.io :as io]
    [clojure.data.generators :as generators]
    [clojusc.twig :refer [pprint]]
    [dragon.blog.content.core :as content]
    [dragon.blog.core :as blog]
    [dragon.selmer.core :refer [render]]
    [starship.blog.web.content.data :as data]
    [taoensso.timbre :as log]
    [trifl.fs :as fs]))

(def new-post-file "new-post.txt")

(defn phrases
  [author category]
  {(format "A new Starship Tools post by %s:" author) 1
   "Have you seen the latest post on the Starship Tools blog?" 1
   "New blog post:" 1
   "There's a new Starship Tools post!" 1
   "We have a new Starship Tools post available:" 2
   "We've put a new Starship Tools post up on the blog:" 3
   "There's a new Starship Tools blog post:" 3
   (format "%s's written a new post for the blog:" author) 4
   "A new Starship Tools post has been published:" 4
   (format "We've got a new '%s' post available:" category) 6})

(defn get-phrase
  [author category]
  (generators/weighted (phrases author category)))

(defn get-content-filename
  [post-data filename]
  (format "%s/%s" (:src-dir post-data) filename))

(defn get-new-post-filename
  [post-data]
  (get-content-filename post-data new-post-file))

(defn render-template
  [system post-data]
  (render
    (str "templates/social/" new-post-file)
    (data/post system [] post-data)))

(defn get-new-post-social-content
  [system post-data]
  (str (get-phrase (:author post-data) (:category post-data))
       (render-template system post-data)))

(defn gen-new-post-social
  [system post-data]
  (let [file-data (get-new-post-social-content system post-data)
        outfile (get-new-post-filename post-data)]
    (when-not (fs/file-exists? (io/file outfile))
      (log/debug "Generating 'new post' social content for" post-data)
      (spit
        outfile
        (get-new-post-social-content system post-data)))))

(defn gen
  [system posts]
  (log/debug "Generating social content ...")
  (log/trace "Got data:" (pprint (blog/data-for-logs posts)))
  (doseq [post-data posts]
    (gen-new-post-social system post-data))
  :ok)
