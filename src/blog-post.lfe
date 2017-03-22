(defmodule blog-post
  (export
    (process-all 1)
    (get-posts 1)
    (process 1)
    (compare-posts-asc 2)
    (compare-posts-desc 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun process-all (posts)
  (clj:->> posts
           (lists:map #'blog-post:process/1)
           (lists:sort #'blog-post:compare-posts-desc/2)))

(defun get-posts (src-dir)
  (clj:-> src-dir
          (filename:join  "*/*/*")
          (filelib:wildcard)))

(defun process (filename)
  (clj:-> filename
          (blog-util:read-file)
          (blog-ct-rfc822:parse)
          (add-post-data)
          (add-counts)
          (add-content-conversions)
          (add-file-data filename)
          (add-date-data)))

(defun compare-posts-asc (post-a post-b)
  (< (proplists:get_value 'date-int post-a)
     (proplists:get_value 'date-int post-b)))

(defun compare-posts-desc (post-a post-b)
  (> (proplists:get_value 'date-int post-a)
     (proplists:get_value 'date-int post-b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Data Transformation Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun add-content-conversions (data)
  (if (md? data)
    (++ data `(#(content ,(parse-markdown data))))
    data))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Utility Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-markdown (data)
  (clj:-> data
          (clj:get-in '(body))
          (binary_to_list)
          (markdown:conv)))

(defun md? (data)
  (case (clj:get-in data '(headers content-type))
    (#"md" 'true)
    (_ 'false)))

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

