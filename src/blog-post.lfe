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

(defun get-categories (posts)
  (get posts 'category))

(defun get-tags (posts)
  (clj:->> (get posts 'tags)
           (lists:foldl #'++/2 '())
           (sets:from_list)
           (sets:to_list)
           (lists:sort)))

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
  (let ((content (clj:get-in data '(body))))
    (++ data
        `(#(charcount ,(blog-util:count-chars content))
          #(wordcount ,(blog-util:count-words content))
          #(linecount ,(blog-util:count-lines content))))))

(defun add-content-conversions (data)
  (if (md? data)
    (++ data
      `(#(content ,(parse-markdown data))
        #(contentbuffer ,(->content-buffer data))))
    data))

(defun add-file-data (data filename)
  (let ((title (->safe-title data))
        (datepath (blog-util:filename->path filename)))
    (++ data
        `(#(srcfile ,filename)
          #(dstfile ,(filename:join
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
          #(datestamp ,(->datestamp data))
          #(timestamp ,(->timestamp data))
          #(dateint ,(->date-int data))
          #(monthname ,(blog-util:month-name m))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Utility Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get (posts key)
  (clj:->> posts
           (lists:map
             (lambda (x)
               (clj:get-in x `(,key))))
           (sets:from_list)
           (sets:to_list)
           (lists:sort)))

(defun parse-markdown (data)
  (clj:-> data
          (clj:get-in '(body))
          (binary_to_list)
          (markdown:conv)))

(defun md? (data)
  (case (clj:get-in data '(headers content-type))
    (#"md" 'true)
    (_ 'false)))

(defun ->content-buffer (data)
  (let ((max-buffer 10)
        (line-count (clj:get-in data '(linecount))))
    (lists:seq 1
      (if (> line-count max-buffer)
          0
          (- max-buffer line-count)))))

(defun ->safe-title (data)
  (clj:->> data
           (proplists:get_value 'title)
           (blog-util:title->file)))

(defun ->date (data)
  (clj:->> data
           (proplists:get_value 'datepath)
           (blog-util:datepath->date)))

(defun ->datestamp (data)
  (clj:->> data
           (proplists:get_value 'datepath)
           (blog-util:datepath->datestamp)))

(defun ->timestamp (data)
  (clj:->> data
           (proplists:get_value 'datepath)
           (blog-util:datepath->timestamp)))

(defun ->date-int (data)
  (clj:->> data
           (proplists:get_value 'datepath)
           (blog-util:datepath->date-int)))

(defun parse-tags (field-body)
  (re:split field-body #",\s*"))

