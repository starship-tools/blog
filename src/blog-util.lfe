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

(defun group-by (func data)
  (group-by
    func
    (lambda (x)
      `#(,(funcall func x) ,x))
    data))

(defun group-by (by-func key-func data)
  (clj:->> data
           (lists:sort (lambda (a b)
                         (> (funcall by-func a)
                            (funcall by-func b))))
           (lists:map (lambda (x)
                        (funcall key-func x)))
           (lists:foldr (match-lambda ((`#(,k ,v) data)
                          (orddict:append k v data)))
                        (orddict:new))
           (orddict:to_list)))

(defun group-by-years (data)
  (group-by
    (lambda (x)
      (clj:get-in x '(year)))
    data))

(defun group-by-months (data)
  (group-by
    (lambda (x)
      (clj:get-in x '(month)))
    data))

(defun group-months-posts (data)
  (lists:map
    (match-lambda ((`#(,month ,months))
      `(#(month ,month)
        #(posts ,months))))
    (group-by-months data)))

(defun group-years-months-posts (data)
  (lists:map
    (match-lambda ((`#(,year ,data))
      `(#(year ,year)
        #(months ,(group-months-posts data)))))
    (group-by-years data)))

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

(defun filter-empty
  (('()) 'false)
  ((_) 'true))

(defun title->file (title)
  (clj:-> title
          (re:replace "[^a-zA-Z]" "-" `(global #(return list)))
          (re:replace "-+" "-" `(global #(return list)))
          (string:to_lower)
          (++ ".html")))

(defun rev-format (data str)
  (io_lib:format str data))

(defun count-chars (data)
  (length (re:split data #".")))

(defun count-lines (data)
  (length (re:split data #"\n")))

(defun count-words (data)
  (length (re:split data #"[^\s]+")))
