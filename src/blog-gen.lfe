(defmodule blog-gen
  (export all))

(defun run ()
  (poise:generate (blog-routes:site)))
