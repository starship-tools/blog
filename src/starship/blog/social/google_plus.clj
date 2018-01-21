(ns starship.blog.social.google-plus
  (:require
    [clojure.data.json :as json]
    [clojure.java.io :as io]
    [clojure.string :as string]
    [dragon.blog.content.core :as content]
    [starship.blog.social.content :as social-content]
    [starship.blog.util :as util]
    [taoensso.timbre :as log]
    [trifl.fs :as fs])
  (:import
    (com.google.api.client.googleapis.auth.oauth2 GoogleCredential)
    (com.google.api.client.googleapis.auth.oauth2 GoogleCredential$Builder)
    (com.google.api.client.http HttpTransport)
    (com.google.api.client.http.javanet NetHttpTransport)
    (com.google.api.client.json JsonFactory)
    (com.google.api.client.json.jackson2 JacksonFactory)
    (com.google.api.services.plusDomains PlusDomains)
    (com.google.api.services.plusDomains PlusDomains$Builder)
    (com.google.api.services.plusDomains.model
     Acl Activity Activity$PlusDomainsObject Person
     PlusDomainsAclentryResource)))

(def app-name "Google+ Blog Updates")
(def community-id "111725368799664699976")
(def community-url "https://plus.google.com/communities/" community-id)
(def scopes ["https://www.googleapis.com/auth/plus.me"
             "https://www.googleapis.com/auth/plus.stream.write"])
(def creds-file "~/.google/starship-tools/blog-update-creds.json")

(defn get-author-email
  []
  (util/home-file->str "~/.google/starship-tools/author-email"))

(defn format-key
  [key-str]
  (-> key-str
      (string/replace "_" "-")
      keyword))

(defn get-creds-stream
  []
  (util/home-file->stream creds-file))

(defn get-creds-data
  []
  (json/read-str
    (util/home-file->str creds-file)
    :key-fn format-key))

(defn json->creds
  []
  (GoogleCredential/fromStream (get-creds-stream)))

(defn make-creds
  ([]
    (make-creds (new NetHttpTransport) (new JacksonFactory)))
  ([http-transport json-factory]
    (let [creds-data (get-creds-data)
          client-email (:client-email creds-data)
          private-key (:private-key creds-data)
          pem-file (io/file (util/write-pem-file private-key))]
      (-> (new GoogleCredential$Builder)
          (.setTransport http-transport)
          (.setJsonFactory json-factory)
          (.setServiceAccountId client-email)
          (.setServiceAccountScopes scopes)
          (.setServiceAccountUser (get-author-email))
          (.setServiceAccountPrivateKeyFromPemFile pem-file)
          ;; cleanup
          ((fn [x] (util/delete-pem-file pem-file) x))
          (.build)))))

(defn get-gplus-service
  []
  (let [http-transport (new NetHttpTransport)
        json-factory (new JacksonFactory)
        creds (make-creds http-transport json-factory)]
    (as-> creds data
         (new PlusDomains$Builder http-transport json-factory data)
         (.setApplicationName data app-name)
         (.build data))))

(defn create-public-acl-resource
  []
  (-> (new PlusDomainsAclentryResource)
      (.setType "public")))

(defn create-public-acl
  []
  (-> (new Acl)
      (.setItems [(create-public-acl-resource)])
      (.setDomainRestricted true)))

(defn create-message-object
  [content]
  (-> (new Activity$PlusDomainsObject)
      (.setOriginalContent content)))

(defn create-activity
  [content acl]
  (-> (new Activity)
      (.setObject (create-message-object content))
      (.setAccess acl)))

(defn send-message
  [content]
  (let [gplus (get-gplus-service)
        acl (create-public-acl)
        activity (create-activity content acl)]
    (-> gplus
        (.activities)
        (.insert community-id activity)
        (.execute))))

(defn send-new-post-message
  [post-file]
  (let [post-data (util/get-post-data post-file)
        msg-file (util/get-message-content-file
                   post-file social-content/new-post-file)
        msg-content (slurp msg-file)]
    (send-message msg-content)))

(defn get-me
  []
  (-> (get-gplus-service)
      (.people)
      (.get "me")
      (.execute)))
