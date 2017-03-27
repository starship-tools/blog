(defmodule blog-pages
  (export all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Static Pages   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun about ()
  (clj:-> (blog-data:about)
          (about-tmpl:render)
          (get-content)))

(defun timeline ()
  (clj:-> (blog-data:timeline)
          (starship-timeline-tmpl:render)
          (get-content)))

(defun search ()
  (clj:-> (blog-data:search)
          (search-tmpl:render)
          (get-content)))

(defun design ()
  (clj:-> (blog-data:design)
          (design-tmpl:render)
          (get-content)))

(defun bootstrap-theme ()
  (clj:-> (blog-data:bootstrap-theme)
          (bootstrap-theme-tmpl:render)
          (get-content)))

(defun example-one-column ()
  (clj:-> (blog-data:example-one-column)
          (example-one-column-tmpl:render)
          (get-content)))

(defun example-two-column ()
  (clj:-> (blog-data:example-two-column)
          (example-two-column-tmpl:render)
          (get-content)))

(defun pages ()
  (clj:-> (blog-data:pages)
          (pages-tmpl:render)
          (get-content)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Dynamic Pages   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun landing (data)
  (clj:-> (blog-data:landing data)
          (landing-tmpl:render)
          (get-content)))

(defun archives (data)
  (clj:-> (blog-data:archives data)
          (archives-tmpl:render)
          (get-content)))

(defun categories (data)
  (clj:-> (blog-data:categories data)
          (categories-tmpl:render)
          (get-content)))

(defun tags (data)
  (clj:-> (blog-data:tags data)
          (tags-tmpl:render)
          (get-content)))

(defun authors (data)
  (clj:-> (blog-data:authors data)
          (authors-tmpl:render)
          (get-content)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Posts   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun post (data)
  (clj:-> (blog-data:post data)
          (post-tmpl:render)
          (get-content)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Utility Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-fragment-page (name)
  (clj:->> name
           (io_lib:format "html-fragments/~s")
           (blog-util:read-priv-file)))

(defun get-content
  "This is a wrapper function that processes the result of an ErlyDTL template
  rendering, returning just the content."
  ((`#(ok ,page)) page)
  ((err) err))
