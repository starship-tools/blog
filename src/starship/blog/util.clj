(ns starship.blog.util
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string]
    [dragon.blog.content.core :as content]
    [taoensso.timbre :as log]
    [trifl.fs :as fs])
  (:import
    (java.io
      ByteArrayInputStream)))

(defn zip
  [& colls]
  (partition (count colls)
             (apply interleave colls)))

(defn get-post-data
  [post-file]
  (log/debugf "Getting content for %s ..." post-file)
  (->> post-file
       (io/resource)
       (.getFile)
       (io/file)
       (content/parse nil)))

(defn get-message-content-file
  [post-file msg-filename]
  (let [parent-dir (fs/parent (io/file post-file))]
    (->> msg-filename
         (format "%s/%s" parent-dir)
         (io/resource)
         (.getFile)
         (io/file))))

(defn read-home-file
  [^String file-name]
  (-> file-name
      fs/expand-home
      io/file))

(defn home-file->str
  [^String file-name]
  (-> file-name
      read-home-file
      slurp
      string/trim-newline))

(defn home-file->stream
  [^String file-name]
  (-> file-name
      read-home-file
      io/input-stream))

(defn str->stream
  [data]
  (->> data
       (map int)
       (byte-array)
       (io/input-stream)))

(defn str->reader
  [data]
  (->> data
       (str->stream)
       (io/reader)))

(defn write-pem-file
  [data]
  (fs/write-tmp-file! "key.pem" data))

(defn delete-pem-file
  [file-obj]
  (io/delete-file file-obj)
  (-> file-obj
      (fs/parent)
      (io/file)
      (io/delete-file)))
