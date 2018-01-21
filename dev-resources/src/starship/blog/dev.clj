(ns starship.blog.dev
  "Blog development namespace

  This namespace is particularly useful when doing active development on the
  blog application."
  (:require
    [cheshire.core :as json]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.java.shell :as shell]
    [clojure.math.combinatorics :refer [cartesian-product]]
    [clojure.pprint :refer [pprint print-table]]
    [clojure.reflect :refer [reflect]]
    [clojure.string :as string]
    [clojure.tools.namespace.repl :as repl]
    [clojure.walk :refer [macroexpand-all]]
    [clojusc.twig :as logger]
    [com.stuartsierra.component :as component]
    [datomic.client :as datomic]
    [dragon.blog.content.block :as block]
    [dragon.blog.content.rfc5322 :as rfc5322]
    [dragon.blog.core :as blog]
    [dragon.blog.generator :as gen]
    [dragon.blog.post.core :as post]
    [dragon.cli.core :as dragon-cli]
    [dragon.components.core :as component-api]
    [dragon.components.system :as components]
    [dragon.config.core :as config]
    [dragon.data.sources.core :as data-source]
    [dragon.data.sources.impl.redis :as redis-db]
    [dragon.dev.system :as dev-system]
    [dragon.main :as dragon-main]
    [dragon.selmer.tags.flickr :as flickr]
    [dragon.util :as dragon-util]
    [ltest.core :as ltest]
    [markdown.core :as md]
    [starship.blog.cli.core :as cli]
    [starship.blog.components.system :as system]
    [starship.blog.core :as core]
    [starship.blog.email.content :as email-content]
    [starship.blog.email.delivery :as email-delivery]
    [starship.blog.main :as main]
    [starship.blog.reader :as reader]
    [starship.blog.routes :as routes]
    [starship.blog.social.content :as social-content]
    [starship.blog.social.google-plus :as gplus]
    [starship.blog.social.twitter :as twitter]
    [starship.blog.util :as util]
    [starship.blog.web.content.data :as data]
    [starship.blog.web.content.page :as page]
    [selmer.parser :as selmer]
    [taoensso.carmine :as car :refer [wcar]]
    [taoensso.timbre :as log]
    [trifl.core :refer [->int]]
    [trifl.fs :as fs]
    [trifl.java :refer [show-methods]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Initial Setup & Utility Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logger/set-level! ['starship.blog 'dragon] :info)

(dev-system/set-generator-ns "starship.blog.core")
(dev-system/set-system-ns "starship.blog.components.system")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   State Management   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn startup
  ([]
    (startup :web))
  ([mode]
    (dev-system/startup mode)))

(def shutdown #'dev-system/shutdown)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Reloading Management   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reset
  []
  (dev-system/shutdown)
  (repl/refresh :after 'starship.blog.dev/startup))

(def refresh #'repl/refresh)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Data   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def redis #'dev-system/redis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Utility Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def show-lines-with-error #'dev-system/show-lines-with-error)
(def show-posts #'dev-system/show-posts)
(def generate #'dev-system/generate)
(def force-regenerate #'dev-system/force-regenerate)
