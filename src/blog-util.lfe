(defmodule blog-util
  (export all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Files &c.   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Types   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bin->atom (bin)
  (clj:-> bin
          (binary_to_list)
          (string:to_lower)
          (list_to_atom)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Groups   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun group-by (by-func key-func comp-func data)
  (clj:->> data
           (lists:sort (lambda (a b)
                         (funcall
                            comp-func
                            (funcall by-func a)
                            (funcall by-func b))))
           (lists:map (lambda (x)
                        (funcall key-func x)))
           (lists:foldr (match-lambda ((`#(,k ,v) data)
                          (orddict:append k v data)))
                        (orddict:new))
           (orddict:to_list)))

;;; Ascending Order

(defun group-by-asc (func data)
  (group-by-asc
    func
    (lambda (x)
      `#(,(funcall func x) ,x))
    data))

(defun group-by-asc (by-func key-func data)
  (group-by by-func key-func #'>/2 data))

(defun group-by-key-asc (data key)
  (group-by-asc
    (lambda (x)
      (clj:get-in x `(,key)))
    data))

(defun group-by-year-asc (data)
  (group-by-key-asc data 'year))

(defun group-by-month-asc (data)
  (group-by-key-asc data 'month))

(defun group-by-category-asc (data)
  (group-by-key-asc data 'category))

(defun group-by-author-asc (data)
  (group-by-key-asc data 'author))

(defun group-month-posts-asc (data)
  (lists:map
    (match-lambda ((`#(,month ,months))
      `(#(month ,month)
        #(posts ,months))))
    (group-by-month-asc data)))

(defun group-category-posts-asc (data)
  (lists:map
    (match-lambda ((`#(,cat ,posts))
      `(#(category ,cat)
        #(posts ,posts))))
    (group-by-category-asc data)))

(defun group-tag-posts-asc (data)
  (lists:map
    (lambda (tag)
      `(#(tag ,tag)
        #(posts , (lists:filter
                    (lambda (x)
                      (lists:member tag (clj:get-in x '(tags))))
                    data))))
    (get-tags data)))

(defun group-author-posts-asc (data)
  (lists:map
    (match-lambda ((`#(,author ,posts))
      `(#(author ,author)
        #(posts ,posts))))
    (group-by-author-asc data)))

(defun group-years-months-posts-asc (data)
  (lists:map
    (match-lambda ((`#(,year ,data))
      `(#(year ,year)
        #(months ,(group-month-posts-asc data)))))
    (group-by-year-asc data)))

;;; Descending Order

(defun group-by-desc (func data)
  (group-by-desc
    func
    (lambda (x)
      `#(,(funcall func x) ,x))
    data))

(defun group-by-desc (by-func key-func data)
  (clj:->> data
           (group-by by-func key-func #'=</2)
           (lists:reverse)))

(defun group-by-key-desc (data key)
  (group-by-desc
    (lambda (x)
      (clj:get-in x `(,key)))
    data))

(defun group-by-year-desc (data)
  (group-by-key-desc data 'year))

(defun group-by-month-desc (data)
  (group-by-key-desc data 'month))

(defun group-month-posts-desc (data)
  (lists:map
    (match-lambda ((`#(,month ,months))
      `(#(month ,month)
        #(posts ,months))))
    (group-by-month-desc data)))

(defun group-years-months-posts-desc (data)
  (lists:map
    (match-lambda ((`#(,year ,data))
      `(#(year ,year)
        #(months ,(group-month-posts-desc data)))))
    (group-by-year-desc data)))

(defun partition-rows-cols (rows cols data)
  (let ((data-len (length data)))
    (lists:map
      (lambda (row)
        (lists:sublist data (+ 1 (* cols (- row 1))) cols))
      (lists:seq 1 rows))))

(defun group-headlines (data)
  (group-headlines 2 data))

(defun group-headlines (rows data)
  (group-headlines 2 2 data))

(defun group-headlines (rows cols data)
  (partition-rows-cols rows cols data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Extraction   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get (posts key)
  (clj:->> posts
           (lists:map
             (lambda (x)
               (clj:get-in x `(,key))))
           (sets:from_list)
           (sets:to_list)
           (lists:sort)))

(defun get-categories (posts)
  (get posts 'category))

(defun get-tags (posts)
  (clj:->> (get posts 'tags)
           (lists:foldl #'++/2 '())
           (sets:from_list)
           (sets:to_list)
           (lists:sort)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Dates   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun filename->path (filename)
  (clj:-> filename
          (re:replace "posts/" "" `(#(return list)))
          (re:replace "\/content.*" "" `(#(return list)))))

(defun datepath->date (datepath)
  (let* ((`(,y ,m ,d ,HMS) (re:split datepath "[^\\d]" `(#(return list))))
         (`(,H ,M ,S) (split-time HMS)))
    `(,y ,m ,d ,H ,M ,S)))

(defun datepath->datestamp (datepath)
  (clj:-> datepath
          (datepath->date)
          (lists:sublist 3)
          (rev-format "~s-~s-~s")))

(defun datepath->timestamp (datepath)
  (clj:-> datepath
          (datepath->date)
          (lists:sublist 4 6)
          (rev-format "~s:~s:~s")))

(defun datepath->date-int (datepath)
  (clj:-> datepath
          (re:replace "[^\\d]" "" `(global #(return list)))
          (list_to_integer)))

(defun split-time (time)
  (lists:filter
    #'filter-empty/1
    (re:split time "(..)" `(trim #(return list)))))

(defun month-name
  (("01") "January")
  (("02") "February")
  (("03") "March")
  (("04") "April")
  (("05") "May")
  (("06") "June")
  (("07") "July")
  (("08") "August")
  (("09") "September")
  (("20") "October")
  (("11") "November")
  (("12") "December"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Predicates   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun filter-empty
  (('()) 'false)
  ((_) 'true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Misc.   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun title->file (title)
  (clj:-> title
          (re:replace "[^a-zA-Z]" "-" `(global #(return list)))
          (re:replace "-+" "-" `(global #(return list)))
          (string:to_lower)
          (++ ".html")))

(defun rev-format (data str)
  (io_lib:format str data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Counts   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun count-chars (data)
  (length (re:split data #".")))

(defun count-lines (data)
  (length (re:split data #"\n")))

(defun count-words (data)
  (length (re:split data #"[^\s]+")))
