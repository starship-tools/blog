(ns starship.blog.main
  (:require
    [clojusc.twig :as logger]
    [dragon.components.system :as components]
    [dragon.config.core :as config]
    [dragon.main :as dragon]
    [starship.blog.cli.core :as cli]
    [starship.blog.core :as core]
    [taoensso.timbre :as log]
    [trifl.java :as trifl])
  (:gen-class))

(defn -main
  "This is the entry point for the blog application.

  It manages both the running of the application and related services, as well
  as use of the application name spaces for running tasks on the comand line.

  The entry point is executed from the command line when calling `lein run`."
  [mode & raw-args]
  ;; let's use twig right away, so any logging is colored/matching once logging
  ;; system is set up. We'll set to the least verbose mode at first, though:
  (logger/set-level! '[mx.roads dragon] :fatal)
  (let [args (dragon/get-default-args raw-args)
        system (dragon/get-context-sensitive-system mode args)]
   (log/infof "Running blog application in %s mode ..." mode)
   (log/debug "Inialized with system:" system)
   (cli/run system args)
   ;; Do a full shut-down upon ^c
   (trifl/add-shutdown-handler #(components/stop system))))
