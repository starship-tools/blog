(defmodule blog-feed
  (export
    (atom 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   API   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun atom (route posts)
  (clj:->> posts
           (atom-data route)
           (atom.xml:render)
           (blog-pages:get-content)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Utility & Helper Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun atom-data (route posts)
  (logjam:info "Generating atom feed data ...")
  (let* ((domain (blog-cfg:domain))
         (base-url (io_lib:format "http://~s" `(,domain))))
    `(#(title ,(blog-cfg:site-title))
      #(subtitle ,(blog-cfg:site-description))
      #(feedurl ,(io_lib:format "~s/~s" `(,base-url ,route)))
      #(baseurl ,base-url)
      #(id ,(io_lib:format "~s:atom:feed" `(,(blog-cfg:domain-urn))))
      #(updated ,(clj:get-in (car posts) '(datetimestamp)))
      #(entries ,(lists:map
                   (lambda (post)
                     (atom-entry base-url post))
                   posts)))))

(defun atom-entry (base-url post)
  (let* ((path (clj:get-in post '(dstfile)))
         (url (io_lib:format "http://~s/~s" `(,base-url ,path))))
    (logjam:info " * Generating feed data for ~s ..." `(,url))
    `(#(title ,(clj:get-in post '(title)))
      #(url ,url)
      #(id ,(get-urn (clj:get-in post '(title))))
      #(updated ,(clj:get-in post '(datetimestamp)))
      #(excerpt ,(clj:get-in post '(excerpt)))
      #(content ,(clj:get-in post '(content)))
      #(author ,(clj:get-in post '(author)))
      #(authorname ,(clj:get-in post '(author)))
      ;;#(authoremail ,(clj:get-in post '(author-email)))
      )))

(defun get-urn (title)
  (clj:->> title
           (blog-util:sanitize-title)
           (list (blog-cfg:domain-urn))
           (io_lib:format "~s:atom:feed:post:~s")))
