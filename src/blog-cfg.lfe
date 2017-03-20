(defmodule blog-cfg
  (export all))

(defun site-title ()
  (site-title (load-config)))

(defun site-title (cfg)
  (get-in '(dragon blog title) cfg))

(defun site-description ()
  (site-description (load-config)))

(defun site-description (cfg)
  (get-in '(dragon blog description) cfg))

(defun posts-src-dir ()
  (posts-src-dir (load-config)))

(defun posts-src-dir (cfg)
  (get-in '(dragon blog posts-src-dir) cfg))

(defun posts-dst-dir ()
  (posts-dst-dir (load-config)))

(defun posts-dst-dir (cfg)
  (get-in '(dragon blog posts-dst-dir) cfg))

(defun output-dir ()
  (output-dir (load-config)))

(defun output-dir (cfg)
  (get-in '(dragon blog output-dir) cfg))

(defun load-config ()
  (lcfg-file:parse-local))

(defun get-in (keys cfg)
  (lists:foldl #'get/2 cfg keys))

(defun get (key config)
  ;; Note that when a keyfind returns false, we need to return an empty list
  ;; so that get-in's foldl will work at any depth.
  (case (lists:keyfind key 1 config)
    ('false '())
    (result (element 2 result))))
