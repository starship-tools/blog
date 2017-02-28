(defmodule blog-cli
  (export all))

(defun do-cmd (cmd)
  (blog:start)
  (funcall cmd)
  (blog:stop))

(defun gen ()
  (do-cmd #'blog-gen:run/0))

(defun start-httpd ()
  (do-cmd #'blog-httpd:start/0))

(defun gen-httpd ()
  (do-cmd
    (lambda ()
      (blog-gen:run)
      (blog-httpd:start))))

(defun gen-watch ()
  (blog-watcher:start))
