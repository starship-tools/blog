(defmodule blog-util
  (export all))

(defun get-priv-dir ()
  (code:priv_dir 'blog))

(defun get-priv-file (path)
  (filename:join `(,(get-priv-dir) ,path)))

(defun read-priv-file (path)
  (read-file (get-priv-file path)))

(defun read-file (path)
  (case (file:read_file path)
    (`#(ok ,data) data)
    (err err)))

(defun bin->atom (bin)
  (clj:-> bin
          (binary_to_list)
          (string:to_lower)
          (list_to_atom)))

(defun count-chars (data)
  (length (re:split data #".")))

(defun count-words (data)
  (length (re:split data #"[^\s]+")))
