(defmodule blog-pages
  (export all))

(defun get-fragment-page (name)
  (clj:->> name
           (io_lib:format "html-fragments/~s")
           (blog-util:read-priv-file)))

(defun landing ()
  (clj:-> (blog-data:landing)
          (landing-tmpl:render)
          (get-content)))

(defun archives ()
  (clj:-> (blog-data:archives)
          (blank-tmpl:render)
          (get-content)))

(defun categories ()
  (clj:-> (blog-data:categories)
          (blank-tmpl:render)
          (get-content)))

(defun tags ()
  (clj:-> (blog-data:tags)
          (blank-tmpl:render)
          (get-content)))

(defun authors ()
  (clj:-> (blog-data:authors)
          (blank-tmpl:render)
          (get-content)))

(defun about ()
  (clj:-> (blog-data:about)
          (about-tmpl:render)
          (get-content)))

(defun pages ()
  (clj:-> (blog-data:pages)
          (pages-tmpl:render)
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

(defun get-content
  "This is a wrapper function that processes the result of an ErlyDTL template
  rendering, returning just the content."
  ((`#(ok ,page)) page)
  ((err) err))
