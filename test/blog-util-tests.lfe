(defmodule blog-util-tests)

(include-lib "ltest/include/ltest-macros.lfe")

(defun group-by-input ()
  '((#(a 1) #(b 2) #(c 3))
    (#(a 1) #(b 22) #(c 33))
    (#(a 1) #(b 222) #(c 333))
    (#(a 11) #(b 22) #(c 33))
    (#(a 111) #(b 22) #(c 33))
    (#(a 111) #(b 222) #(c 333))))

(defun group-by-expected ()
  '(#(1
      ((#(a 1) #(b 2) #(c 3))
       (#(a 1) #(b 22) #(c 33))
       (#(a 1) #(b 222) #(c 333))))
    #(11
      ((#(a 11) #(b 22) #(c 33))))
    #(111
      ((#(a 111) #(b 22) #(c 33))
       (#(a 111) #(b 222) #(c 333))))))

(deftest group-by
  (is-equal (blog-util:group-by
              (lambda (x)
                (proplists:get_value 'a x))
              (group-by-input))
            (group-by-expected)))
