(defmodule blog-post
  (export all))

(defun process (filename)
  (clj:-> filename
          (blog-util:read-file)
          (blog-ct-rfc822:parse)
          (add-post-data)
          (add-file-data filename)
          (add-date-data)
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

(defun add-file-data (data filename)
  (let ((title (->safe-title data))
        (datepath (blog-util:filename->path filename)))
    (++ data
        `(#(src-file ,filename)
          #(dst-file ,(filename:join
                        (list (blog-cfg:posts-dst-dir) datepath title)))
          #(datepath ,datepath)))))

(defun add-date-data (data)
  (let ((`(,y ,m ,d ,H ,M ,S) (->date data)))
    (++ data
        `(#(year ,y)
          #(month ,m)
          #(day ,d)
          #(hour ,H)
          #(minute ,M)
          #(second ,S)
          #(date-int ,(->date-int data))
          #(month-name ,(blog-util:month-name m))))))

(defun add-counts (data)
  (++ data
      `(#(char-count ,(blog-util:count-chars (clj:get-in data '(body))))
        #(word-count ,(blog-util:count-words (clj:get-in data '(body)))))))

;;; Utility functions

(defun compare-posts-asc (post-a post-b)
  (< (proplists:get_value 'date-int post-a)
     (proplists:get_value 'date-int post-b)))

(defun compare-posts-desc (post-a post-b)
  (> (proplists:get_value 'date-int post-a)
     (proplists:get_value 'date-int post-b)))

(defun ->safe-title (data)
  (clj:->> data
           (proplists:get_value 'title)
           (blog-util:title->file)))

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

