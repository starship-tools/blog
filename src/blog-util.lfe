(defmodule blog-util
  (export all))

(defun get-priv-dir ()
  (code:priv_dir 'blog))
