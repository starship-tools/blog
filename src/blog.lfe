(defmodule blog
  (behaviour gen_server)
  (export
    ;; blog functions
    (watch 0)
    (process 0)
    (get-posts 0)
    (routes 0)
    (gen 0)
    ;; gen_server implementation
    (start 0)
    (start-gen-server 0)
    (start-gen-server 1)
    (stop 0)
    ;; callback implementation
    (init 1)
    (handle_call 3)
    (handle_cast 2)
    (handle_info 2)
    (terminate 2)
    (code_change 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Config Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun server-name () (MODULE))
(defun callback-module () (MODULE))
(defun initial-state () (blog-cfg:load-config))
(defun genserver-opts () '())
(defun register-name () `#(local ,(server-name)))
(defun unknown-command () #(error "Unknown command."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Blog API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun watch ()
  (blog-watcher:start))

(defun process ()
  (blog-post:process-all (get-posts)))

(defun get-posts ()
  (blog-post:get-posts
    (blog-cfg:posts-src-dir)))

(defun routes ()
  (blog-routes:routes))

(defun gen ()
  (equipoise:generate
    (routes)
    (map 'output-dir (blog-cfg:output-dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   gen_server Implementation   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start ()
  (start "Starting blog gen-server ..."))

(defun start (msg)
  (start #'logjam:info/1 msg))

(defun start (log-fn log-msg)
  (logjam:start)
  (application:ensure_all_started 'blog)
  (funcall log-fn `(,log-msg))
  (start-gen-server))

(defun start-gen-server ()
  (start-gen-server (initial-state)))

(defun start-gen-server (cfg)
  (gen_server:start (register-name)
                    (callback-module)
                    cfg
                    (genserver-opts)))

(defun stop ()
  (stop "Stopping blog gen-server ..."))

(defun stop (msg)
  (stop #'logjam:info/1 msg))

(defun stop (log-fn log-msg)
  (funcall log-fn `(,log-msg))
  (stop-gen-server)
  (application:stop 'blog))

(defun stop-gen-server ()
  (gen_server:call (server-name) 'stop)
  (gen_server:stop (server-name)))

(defun restart ()
  (stop (lambda (x) x) "")
  (start "Restarting blog gen-server ..."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Callback Implementation   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init (initial-state)
  `#(ok ,initial-state))

(defun handle_cast
  ((message state-data)
    `#(reply ,(unknown-command) ,state-data)))

(defun handle_call
  (('stop caller state-data)
    `#(reply stopping ,state-data))
  ((message _caller state-data)
    `#(reply ,(unknown-command) ,state-data)))

(defun handle_info
  ((`#(EXIT ,_pid normal) state-data)
   `#(noreply ,state-data))
  ((`#(EXIT ,pid ,reason) state-data)
   (logjam:error "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state-data))
  ((_msg state-data)
   `#(noreply ,state-data)))

(defun terminate (reason _state-data)
  (logjam:info "Application terminating: ~p" `(,reason))
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))
