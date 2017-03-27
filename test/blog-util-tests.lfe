(defmodule blog-util-tests)

(include-lib "ltest/include/ltest-macros.lfe")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Tests   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest group-by
  (is-equal (blog-util:group-by
              (lambda (x)
                (proplists:get_value 'a x))
              (group-by-input-1))
            (group-by-expected-1)))

(deftest group-by-year
  (is-equal
    (blog-util:group-by-year
      (group-by-input-2))
    (group-by-expected-2)))

(deftest group-by-month
  (is-equal
    (blog-util:group-by-month
      (group-by-input-2))
    (group-by-expected-3)))

(deftest group-years-months-posts
  (is-equal
    (blog-util:group-years-months-posts
      (group-by-input-2))
    (group-by-expected-4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Test Data   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun group-by-input-1 ()
  '((#(a 1) #(b 2) #(c 3))
    (#(a 1) #(b 22) #(c 33))
    (#(a 1) #(b 222) #(c 333))
    (#(a 11) #(b 22) #(c 33))
    (#(a 111) #(b 22) #(c 33))
    (#(a 111) #(b 222) #(c 333))))

(defun group-by-input-2 ()
  '((#(year 1999) #(month 1) #(day 1))
    (#(year 1999) #(month 2) #(day 11))
    (#(year 2000) #(month 3) #(day 12))
    (#(year 2001) #(month 4) #(day 17))
    (#(year 2002) #(month 5) #(day 22))
    (#(year 2002) #(month 5) #(day 23))
    (#(year 2002) #(month 6) #(day 30))))

(defun group-by-input-3 ()
  '((#(year 1999) #(month 1) #(day 1) #(tags ("a" "b" "c")))
    (#(year 1999) #(month 2) #(day 11) #(tags ("b")))
    (#(year 2000) #(month 3) #(day 12) #(tags ("a" "c")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Results Data   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun group-by-expected-2 ()
  '(#(1999
      ((#(year 1999) #(month 1) #(day 1)) (#(year 1999) #(month 2) #(day 11))))
    #(2000 ((#(year 2000) #(month 3) #(day 12))))
    #(2001 ((#(year 2001) #(month 4) #(day 17))))
    #(2002
      ((#(year 2002) #(month 5) #(day 22))
       (#(year 2002) #(month 5) #(day 23))
       (#(year 2002) #(month 6) #(day 30))))))

(defun group-by-expected-3 ()
  '(#(1 ((#(year 1999) #(month 1) #(day 1))))
    #(2 ((#(year 1999) #(month 2) #(day 11))))
    #(3 ((#(year 2000) #(month 3) #(day 12))))
    #(4 ((#(year 2001) #(month 4) #(day 17))))
    #(5
      ((#(year 2002) #(month 5) #(day 22)) (#(year 2002) #(month 5) #(day 23))))
    #(6 ((#(year 2002) #(month 6) #(day 30))))))

(defun group-by-expected-4 ()
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
