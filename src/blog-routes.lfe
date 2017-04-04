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
      ,(indexed-posts-routes all-posts)
      ,(feed-routes all-posts))))

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

(defun indexed-posts-routes (all-posts)
  (let* ((count (length all-posts))
         (indexed-posts (lists:zip (lists:seq 1 count) all-posts)))
    (logjam:info "Total post count: ~p" `(,(length all-posts)))
    (lists:map (match-lambda ((`#(,index ,post))
                 (get-indexed-route all-posts count index post)))
               indexed-posts)))

(defun get-route (post-data)
  `(,(proplists:get_value 'dstfile post-data)
    ,(blog-pages:post post-data)))

(defun get-indexed-route (all-posts count index post-data)
  "Get the post route and associate the prev/next posts with the route's
  post data.

  Remember, the first element in the list of posts is the most recent post.
  Previous posts have higher indices, with the oldest post having the highest
  index."
  (let* ((url (proplists:get_value 'dstfile post-data))
         (prev-index (clj:inc index))
         (next-index (clj:dec index))
         (prev-post (if (> prev-index count)
                      ""
                      (proplists:get_value
                        'dstfile
                        (lists:nth prev-index all-posts))))
         (next-post (if (< next-index 1)
                      ""
                      (proplists:get_value
                        'dstfile
                        (lists:nth next-index all-posts)))))
    (logjam:debug "Current index: ~p" `(,index))
    (logjam:debug "Previous index: ~p" `(,prev-index))
    (logjam:debug "Next index: ~p" `(,next-index))
    (logjam:info "Generating route for /~s ..." `(,url))
    `(,url
      ,(blog-pages:post
         (lists:append
           post-data
           `(#(prevpost ,prev-post)
             #(nextpost ,next-post)))))))

(defun feed-routes (data)
  (let ((route "atom.xml"))
    (logjam:info "Generating XML for feeds ...")
    `((,route (,(blog-feed:atom route data))))))
