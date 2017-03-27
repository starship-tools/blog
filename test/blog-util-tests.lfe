(defmodule blog-util-tests)

(include-lib "ltest/include/ltest-macros.lfe")

(defun group-by-input-1 ()
  '((#(a 1) #(b 2) #(c 3))
    (#(a 1) #(b 22) #(c 33))
    (#(a 1) #(b 222) #(c 333))
    (#(a 11) #(b 22) #(c 33))
    (#(a 111) #(b 22) #(c 33))
    (#(a 111) #(b 222) #(c 333))))

(defun group-by-expected-1 ()
  '(#(1
      ((#(a 1) #(b 2) #(c 3))
       (#(a 1) #(b 22) #(c 33))
       (#(a 1) #(b 222) #(c 333))))
    #(11
      ((#(a 11) #(b 22) #(c 33))))
    #(111
      ((#(a 111) #(b 22) #(c 33))
       (#(a 111) #(b 222) #(c 333))))))

(defun group-by-input-2 ()
  '((#(year 1999) #(month 1) #(day 1))
    (#(year 1999) #(month 2) #(day 11))
    (#(year 2000) #(month 3) #(day 12))
    (#(year 2001) #(month 4) #(day 17))
    (#(year 2002) #(month 5) #(day 22))
    (#(year 2002) #(month 5) #(day 23))
    (#(year 2002) #(month 6) #(day 30))))

(defun group-by-expected-2 ()
  '((#(year 1999)
     #(months
       ((#(month 1) #(posts ((#(year 1999) #(month 1) #(day 1)))))
        (#(month 2) #(posts ((#(year 1999) #(month 2) #(day 11))))))))
    (#(year 2000)
     #(months ((#(month 3) #(posts ((#(year 2000) #(month 3) #(day 12))))))))
    (#(year 2001)
     #(months ((#(month 4) #(posts ((#(year 2001) #(month 4) #(day 17))))))))
    (#(year 2002)
     #(months
       ((#(month 5)
         #(posts
           ((#(year 2002) #(month 5) #(day 22))
            (#(year 2002) #(month 5) #(day 23)))))
        (#(month 6) #(posts ((#(year 2002) #(month 6) #(day 30))))))))))

(deftest group-by
  (is-equal (blog-util:group-by
              (lambda (x)
                (proplists:get_value 'a x))
              (group-by-input-1))
            (group-by-expected-1)))

(deftest group-by-nested
  (is-equal
    (group-years-months-posts (group-by-input-2))
    (group-by-expected-2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Testing Support Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun group-by-years (data)
  (blog-util:group-by
    (lambda (x)
      (clj:get-in x '(year)))
    (group-by-input-2)))

(defun group-by-months (data)
  (blog-util:group-by
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
