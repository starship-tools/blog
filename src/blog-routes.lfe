(defmodule blog-routes
  (export all))

(defun static-routes ()
  `(("index.html"
      ,(lambda () (blog-pages:get-page 'landing)))
    ("archives.html"
      ,(lambda () (blog-pages:get-page 'archives)))
    ("categories.html"
      ,(lambda () (blog-pages:get-page 'categories)))
    ("tags.html"
      ,(lambda () (blog-pages:get-page 'tags)))
    ("authors.html"
      ,(lambda () (blog-pages:get-page 'authors)))
    ("pages.html"
      ,(lambda () (blog-pages:get-page 'pages)))
    ("about.html"
      ,(lambda () (blog-pages:get-page 'about)))
    ("starship-timeline.html"
      ,(lambda () (blog-pages:get-page 'timeline)))
    ("search.html"
      ,(lambda () (blog-pages:get-page 'search)))
    ("design.html"
      ,(lambda () (blog-pages:get-page 'design)))
    ("design/bootstrap-theme.html"
      ,(lambda () (blog-pages:get-page 'bootstrap-theme)))
    ("design/example-2-column.html"
      ,(lambda () (blog-pages:get-page 'example-two-column)))))

(defun posts-routes ()
  `())

(defun routes ()
  (lists:append (static-routes) (posts-routes)))

(defun site ()
  "Generate the blog site."
  (poise:site
    (routes)
    `#m(output-dir ,(blog-cfg:output-dir))))
