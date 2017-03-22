(defmodule blog-httpd
  (export
    (start 0)
    (stop 0)
    (restart 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start ()
  (logjam:info "Starting HTTP server ...")
  (-start))

(defun stop ()
  (logjam:info "Stopping HTTP server ...")
  (-stop))

(defun restart ()
  (logjam:info "Restarting HTTP server ...")
  (-stop)
  (-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Support Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handler (request)
  (logjam:info "Handling request for: ~s"
             `(,(element 8 request)))
  request)

(defun -start ()
  (application:ensure_all_started 'inets)
  (barista:start #'handler/1))

(defun -stop ()
  (barista:stop)
  ;; XXX move the following capabilities into the barista library when stopping
  (erlang:unregister 'lmug-handler)
  (application:stop 'inets))
