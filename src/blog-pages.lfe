(defmodule blog-pages
  (export all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Static Pages   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun about ()
  (clj:-> (blog-data:about)
          (about.html:render)
          (get-content)))

(defun timeline ()
  (clj:-> (blog-data:timeline)
          (starship-timeline.html:render)
          (get-content)))

(defun search ()
  (clj:-> (blog-data:search)
          (search.html:render)
          (get-content)))

(defun design ()
  (clj:-> (blog-data:design)
          (design.html:render)
          (get-content)))

(defun bootstrap-theme ()
  (clj:-> (blog-data:bootstrap-theme)
          (bootstrap-theme.html:render)
          (get-content)))

(defun example-one-column ()
  (clj:-> (blog-data:example-one-column)
          (example-one-column.html:render)
          (get-content)))

(defun example-two-column ()
  (clj:-> (blog-data:example-two-column)
          (example-two-column.html:render)
          (get-content)))

(defun pages ()
  (clj:-> (blog-data:pages)
          (pages.html:render)
          (get-content)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Dynamic Pages   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun landing (data)
  (clj:-> (blog-data:landing data)
          (landing.html:render)
          (get-content)))

(defun archives (data)
  (clj:-> (blog-data:archives data)
          (archives.html:render)
          (get-content)))

(defun categories (data)
  (clj:-> (blog-data:categories data)
          (categories.html:render)
          (get-content)))

(defun tags (data)
  (clj:-> (blog-data:tags data)
          (tags.html:render)
          (get-content)))

(defun authors (data)
  (clj:-> (blog-data:authors data)
          (authors.html:render)
          (get-content)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Posts   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun post (data)
  (clj:-> (blog-data:post data)
          (post.html:render)
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
