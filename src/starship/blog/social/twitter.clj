(ns starship.blog.social.twitter
  (:require
    [clojure.java.io :as io]
    [dragon.blog.content.core :as content]
    [dragon.config.core :as config]
    [starship.blog.social.content :as social-content]
    [starship.blog.util :as util]
    [taoensso.timbre :as log]
    [trifl.fs :as fs]
    [twitter.api.restful :as twitter]
    [twitter.oauth :as oath]))

(def screen-name "StarshipTools")

(defn get-creds
  [system]
  (oath/make-oauth-creds (config/twitter-api-app-consumer-key system)
                         (config/twitter-api-app-consumer-secret system)
                         (config/twitter-api-user-access-token system)
                         (config/twitter-api-user-access-secret system)))

(defn send-message
  [system content]
  (twitter/statuses-update
    :oauth-creds (get-creds system)
    :params {:status content}))

(defn send-new-post-message
  [system post-file]
  (let [post-data (util/get-post-data post-file)
        msg-file (util/get-message-content-file
                   post-file social-content/new-post-file)
        msg-content (slurp msg-file)]
    (send-message system msg-content)))

(defn show-friends
  [system]
  (twitter/friendships-show
    :oauth-creds (get-creds system)
    :params {:target-screen-name screen-name}))
