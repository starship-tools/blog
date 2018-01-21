(ns starship.blog.core
  (:require
    [clojusc.twig :as logger]
    [dragon.blog.core :as blog]
    [dragon.blog.generator :as gen]
    [dragon.config.core :as config]
    [dragon.core :as dragon-core]
    [dragon.util :as util]
    [starship.blog.email.content :as email-content]
    [starship.blog.routes :refer [gen-routes routes]]
    [starship.blog.social.content :as social-content]
    [trifl.core :refer [sys-prop]]
    [trifl.docs :as docs]))

(defn version
  []
  (let [version (sys-prop "blog.version")
        build (util/get-build)]
    (format "Starship Tools Blog version %s, build %s\n" version build)))

(defn generate
  [system]
  (let [posts (dragon-core/generate system)
        generated-routes (gen-routes system posts)]
    (gen/run system generated-routes)
    (email-content/gen system posts)
    (social-content/gen system posts)))

;; XXX force-regenerate doesn't seem to be working yet ... however, this works:
;;     `(data-source/set-posts-checksums (get-in system [:db :querier]) "")`
;;     which is odd, since that's what force-regenerate ends up calling ...
(defn force-regenerate
  [system]
  (blog/reset-content-checksums system)
  (generate system))
