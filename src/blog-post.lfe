(defmodule blog-post
  (export all))

(defun process (filename)
  (clj:-> filename
          (blog-util:read-file)
          (blog-ct-rfc822:parse)
          (add-file-data filename)
          (add-date-data)
          (add-post-data)
          (add-counts)))

(defun add-file-data (data filename)
  (++ data
      `(#(filename ,filename)
        #(datepath ,(blog-util:filename->path filename)))))

(defun add-date-data (data)
  (let ((`(,y ,m ,d ,H ,M ,S) (->date data)))
    (++ data
        `(#(year ,y)
          #(month ,m)
          #(day ,d)
          #(hour ,H)
          #(minute ,M)
          #(second ,S)
          #(date-int ,(->date-int data))))))


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

;;; Utility functions

(defun ->date (data)
  (clj:->> data
           (proplists:get_value 'datepath)
           (blog-util:datepath->date)))

(defun ->date-int (data)
  (clj:->> data
           (proplists:get_value 'datepath)
           (blog-util:datepath->date-int)))

(defun parse-tags (field-body)
  (re:split field-body #",\s*"))
