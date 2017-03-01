(defmodule blog-routes
  (export all))

(defun get-routes ()
  `(("index.html"
      ,(lambda () (blog-pages:get-page 'landing)))
    ("search.html"
      ,(lambda () (blog-pages:get-page 'search)))
    ("design/bootstrap-theme.html"
      ,(lambda () (blog-pages:get-page 'bootstrap-theme)))
    ("design/example-2-column.html"
      ,(lambda () (blog-pages:get-page 'example-two-column)))))

(defun site ()
  "Generate the blog site."
  (poise:site
    (get-routes)
    `#m(output-dir "docs")))
