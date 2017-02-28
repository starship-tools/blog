(defmodule blog-watcher
  (export all))

(defun source-data ()
  `#("src" ,#'blog-watcher:lfe-watcher/1))

(defun template-data ()
  `#("priv/templates" ,#'blog-watcher:erlydtl-watcher/1))

(defun default-watch-data ()
  `(,(source-data)
    ,(template-data)))

(defun lfe-watcher
  ((`#(,path file close_write ,fd ,file))
    (blog-compiler:lfe path file (source-data)))
  ((args)
    (logjam:debug "Unhandled LFE watcher event: ~p" `(,args))))

(defun erlydtl-watcher
  ((`#(,path file close_write ,fd ,file))
    (blog-compiler:erlydtl path file (template-data)))
  ((args)
    (logjam:debug "Unhandled ErlyDTL watcher event: ~p" `(,args))))

(defun start ()
  (start (default-watch-data)))

(defun start (watch-data)
  (logjam:start)
  (blog-gen:run-dev)
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
