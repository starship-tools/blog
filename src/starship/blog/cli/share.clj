(ns starship.blog.cli.share
  (:require
    [clojure.pprint :refer [pprint]]
    [clojusc.twig :as logger]
    [dragon.config.core :as config]
    [dragon.util :as util]
    [starship.blog.email.delivery :as email-delivery]
    [starship.blog.social.twitter :as twitter]
    [taoensso.timbre :as log]
    [trifl.docs :as docs]))

(defn run
  "
  Usage:
  ```
    blog share [SUBCOMMAND <post content file> | help]
  ```

  If no SUBCOMMAND is provided, the default 'help' will be used.

  Subcommands:
  ```
    subscribers   Notify email subscribers of the given post
    twitter       Tweet the given post on the Starship Tools Twitter account
    all           Publish to all supported services
  ```

  More information:

    Each command takes an optional 'help' subcommand that will provide
    usage information about how to share, e.g.::

  ```
    $ blog share email help
  ```"
  [system [cmd & [post-file-kw & args]]]
  (if (and (not= cmd :help) (nil? post-file-kw))
    (do
      (log/error "You need to provide a filename.")
      (run system [:help]))
    (let [post-file (name (or post-file-kw :help))]
      (log/debug "Got cmd:" cmd)
      (log/debug "Got post-file:" post-file)
      (log/debug "Got args:" args)
      (case cmd
        :subscribers (email-delivery/send-new-post-message post-file)
        :twitter (twitter/send-new-post-message system post-file)
        ; :google+ ()
        :all (do (email-delivery/send-new-post-message post-file)
                 (twitter/send-new-post-message post-file))
        (docs/print-docstring #'run)))))
