(defmodule blog-post
  (export all))

(defun process (filename)
  (clj:-> filename
          (blog-util:read-file)
          (blog-ct-rfc822:parse)
          (add-post-data)
          (add-counts)))

(defun add-post-data (data)
  (++ data
      `(#(title ,(clj:get-in data '(headers subject)))
        #(subtitle ,(clj:get-in data '(headers subtitle)))
        #(excerpt ,(clj:get-in data '(headers excerpt)))
        #(author ,(clj:get-in data '(headers from)))
        #(category ,(clj:get-in data '(headers category)))
        #(tags ,(parse-tags (clj:get-in data '(headers keywords))))
        #(comments ,(clj:get-in data '(headers comments))))))

(defun add-counts (data)
  (++ data
      `(#(char-count ,(blog-util:count-chars (clj:get-in data '(body))))
        #(word-count ,(blog-util:count-words (clj:get-in data '(body)))))))

(defun parse-tags (field-body)
  (re:split field-body #",\s*"))
