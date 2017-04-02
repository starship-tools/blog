(defmodule blog-routes
  (export
    (routes 0)
    (routes 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun routes ()
  (routes (blog:process)))

(defun routes (all-posts)
  (lists:append
    `(,(static-routes)
      ,(dynamic-routes all-posts)
      ,(posts-routes all-posts))))

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

(defun dynamic-routes (all-posts)
  `(("index.html"      ,(blog-pages:landing all-posts))
    ("archives.html"   ,(blog-pages:archives all-posts))
    ("categories.html" ,(blog-pages:categories all-posts))
    ("tags.html"       ,(blog-pages:tags all-posts))
    ("authors.html"    ,(blog-pages:authors all-posts))))

(defun posts-routes (all-posts)
  (lists:map #'get-route/1 all-posts))

(defun get-route (post-data)
  `(,(proplists:get_value 'dstfile post-data)
    ,(blog-pages:post post-data)))
