;;;; This module provides functions for working with RFC 822 (Internet Message)
;;;; content (the 'ct' in the module name stands for 'content type'). Note that
;;;; the full RFC spec is not supported (e.g., no folding support). This is very
;;;; limited in nature.

(defmodule blog-ct-rfc822
  (export all))

(defun parse (data)
  (let ((`(,headers ,body) (headers-body data)))
  `(#(headers ,(clj:->> headers
                        (parse-headers)
                        (lists:map #'parse-header/1)))
    #(body ,body))))

(defun headers-body (data)
  (re:split data #"\n\n" '(#(parts 2))))

(defun parse-headers (data)
  (re:split data #"\n"))

(defun parse-header (header)
  (let ((`(,field-name ,field-body) (split-header header)))
    `#(,(blog-util:bin->atom field-name) ,field-body)))

(defun split-header (header)
  (let ((results (re:split header #"(\s*)?([^ ]+)(\s*)?:\s*" '(#(parts 2)))))
    `(,(lists:nth 3 results) ,(lists:nth 5 results))))
