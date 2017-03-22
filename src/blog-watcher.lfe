;;;; This module uses the Erlang inotify library which depends upon the
;;;; Linux inotify library. This will not work on systems that do not
;;;; support inotify, and in particular, the Erlang inotify NIF.
(defmodule blog-watcher
  (export
    (start 0) (start 1)
    (stop 0)
    (restart 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start ()
  (start (default-watch-data)))

(defun start (watch-data)
  (logjam:start)
  (blog-gen:run)
  (blog-httpd:start)
  (application:ensure_all_started 'inotify)
  (application:ensure_all_started 'blog)
  (blog:start-gen-server)
  (case (lists:map (match-lambda ((`#(,path ,func))
                     (logjam:info "Watching path: ~p with function: ~p"
                                   `(,path ,func))
                     (inotify:watch path func)))
                   watch-data)
    ('(ok ok) 'ok)
    (err (logjam:error err))))

(defun stop ()
  (blog:stop-gen-server)
  (application:stop 'blog)
  (application:stop 'ets_manager)
  (application:stop 'inotify)
  (blog-httpd:stop)
  (logjam:stop))

(defun restart ()
  (stop)
  (timer:sleep 500)
  (start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Config Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lfe-data ()
  `#("src" ,#'blog-watcher:lfe-watcher/1))

(defun template-data ()
  `#("priv/templates" ,#'blog-watcher:erlydtl-watcher/1))

(defun default-watch-data ()
  `(,(lfe-data)
    ,(template-data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Support Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lfe-watcher
  ((`#(,path file close_write ,fd ,file))
    (blog-compiler:lfe path file (lfe-data)))
  ((args)
    (logjam:debug "Unhandled LFE watcher event: ~p" `(,args))))

(defun erlydtl-watcher
  ((`#(,path file close_write ,fd ,file))
    (blog-compiler:erlydtl path file (template-data)))
  ((args)
    (logjam:debug "Unhandled ErlyDTL watcher event: ~p" `(,args))))
