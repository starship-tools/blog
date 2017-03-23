;;;; This module provides functions for working with RFC 822 (Internet Message)
;;;; content (the 'ct' in the module name stands for 'content type'). Note that
;;;; nowhere near the full RFC spec is supported (e.g., no folding
;;;; support, only lf not crlf, etc) and that this module is very limited in
;;;; nature.
(defmodule blog-ct-rfc822
  (export (parse 1)
          (parse-header 1)
          (split-header 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse (data)
  (let ((`(,headers ,body) (headers-body data)))
  `(#(headers ,(clj:->> headers
                        (parse-headers)
                        (lists:map #'parse-header/1)))
    #(body ,body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Support Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun headers-body (data)
  (re:split data #"\n\n" '(#(parts 2))))

(defun parse-headers (data)
  (re:split data #"\n"))

(defun parse-header (header)
  (let ((`(,field-name ,field-body) (split-header header)))
    `#(,(blog-util:bin->atom field-name) ,field-body)))

(defun split-header (header)
  (let ((results (re:split header (header-separator) '(#(parts 2)))))
    `(,(lists:nth 3 results) ,(lists:nth 5 results))))

(defun header-separator ()
  #"(\s*)?([^ ]+)(\s*)?:\s*")
