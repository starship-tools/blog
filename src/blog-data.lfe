(defmodule blog-data
  (export all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Pages With Top-Nav Entries   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun landing (data)
  (let ((base-data (base)))
    (lists:append
      `(,base-data
        (#(page_title ,(proplists:get_value 'site_title base-data))
         #(page_description ,(proplists:get_value 'site_description base-data))
         #(active "index")
         #(headliner ,(car data))
         #(headlines (list:sublist 2 5)))))))

(defun archives (data)
  (lists:append
    `(,(base)
      (#(page_title "Archives")
       #(active "archives")
       #(postsdata ,(blog-util:group-years-months-posts data))))))

(defun categories (data)
  (lists:append
    `(,(base)
      ,data
      (#(page_title "Categories")
       #(active "categories")))))

(defun tags (data)
  (lists:append
    `(,(base)
      ,data
      (#(page_title "Tags")
       #(active "tags")))))

(defun authors (data)
  (lists:append
    `(,(base)
      ,data
      (#(page_title "Authors")
       #(active "authors")))))

(defun about ()
  (lists:append
    (base)
    `(#(page_title "About")
      #(active "about"))))

(defun pages ()
  (lists:append
    (base)
    `(#(page_title "Pages")
      #(page_description "Some of the content on the blog is maintained
                          as actual pages rather than as posts. You will
                          find links to them here, below.")
      #(active "pages"))))

(defun design ()
  (lists:append
    (base)
    `(#(page_title "Design")
      #(page_description "Various design pages used during site development.
                         These pages will eventually go away.")
      #(active "design"))))

(defun bootstrap-theme ()
  (lists:append
    (base)
    `(#(page_title "Design | Theme Demo")
      #(active "design"))))

(defun example-one-column ()
  (lists:append
    (base)
    `(#(page_title "Design | Example One-column")
      #(active "design"))))

(defun example-two-column ()
  (lists:append
    (base)
    `(#(page_title "Design | Example Two-column")
      #(active "design"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Posts   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun post (data)
  (logjam:debug "Got post data: ~p" `(,data))
  (lists:append
    `(,(base)
      (#(page_title "Article")
       #(postdata ,data)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Non-Top-Nav Pages   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun timeline ()
  (lists:append
    (base)
    `(#(page_title "A Timeline of Starship Programs"))))

(defun search ()
  (lists:append
    (base)
    `(#(page_title "Search"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Support Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun base ()
  `(#(site_title ,(blog-cfg:site-title))
    #(site_description ,(blog-cfg:site-description))
    #(index "index")
    #(landing "landing")
    #(archives "archives")
    #(categories "categories")
    #(tag "tags")
    #(authors "authors")
    #(about "about")
    #(design "design")
    #(pages "pages")))
