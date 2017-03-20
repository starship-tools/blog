(defmodule blog-ct-rfc822-tests)

(include-lib "ltest/include/ltest-macros.lfe")

(defun get-test-data ()
  (clj:-> "sample-files/sample-post.rfc822"
          (blog-util:read-priv-file)))

(deftest parse-header
  (is-equal (blog-ct-rfc822:parse-header "Hey: there")
            #(hey #"there"))
  (is-equal (blog-ct-rfc822:parse-header "hey:   there")
            #(hey #"there"))
  (is-equal (blog-ct-rfc822:parse-header "  HEY  :      THERE!")
            #(hey #"THERE!")))

(deftest split-header
  (is-equal (blog-ct-rfc822:split-header "Hey: there")
            '(#"Hey" #"there"))
  (is-equal (blog-ct-rfc822:split-header "hey:   there")
            '(#"hey" #"there"))
  (is-equal (blog-ct-rfc822:split-header "  HEY  :      THERE!")
            '(#"HEY" #"THERE!")))
