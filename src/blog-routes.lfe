(defmodule blog-routes
  (export
    (routes 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun routes ()
  (let ((data (blog:process)))
    (lists:append
      `(,(static-routes)
        ,(dynamic-routes data)
        ,(posts-routes data)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Route Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun static-routes ()
  `(("pages.html"                   ,(blog-pages:pages))
    ("about.html"                   ,(blog-pages:about))
    ("starship-timeline.html"       ,(blog-pages:timeline))
    ("search.html"                  ,(blog-pages:search))
    ("design.html"                  ,(blog-pages:design))
    ("design/bootstrap-theme.html"  ,(blog-pages:bootstrap-theme))
    ("design/example-1-column.html" ,(blog-pages:example-one-column))
    ("design/example-2-column.html" ,(blog-pages:example-two-column))))

(defun dynamic-routes (data)
  `(("index.html"      ,(blog-pages:landing data))
    ("archives.html"   ,(blog-pages:archives data))
    ("categories.html" ,(blog-pages:categories data))
    ("tags.html"       ,(blog-pages:tags data))
    ("authors.html"    ,(blog-pages:authors data))))

(defun posts-routes (posts)
  (lists:map #'get-route/1 posts))

(defun get-route (post-data)
  `(,(proplists:get_value 'dst-file post-data) ,(blog-pages:about)))
