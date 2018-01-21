(ns starship.blog.routes
  "The routes for the blog need to take into consideration the following:

   * Actual posts will be generated behind the scenes when processing on-disk
     content (e.g., when calling `process-all-by-year-and-month`).
   * The routes are only used durng development, when serving content
     dynamically.
   * Since the posts have already been generated and saved to disc, their
     routes should be generated dynamically as URI path / slurp call pairs."
  (:require
    [clojusc.twig :refer [pprint]]
    [dragon.blog.core :as blog]
    [dragon.config.core :as config]
    [dragon.event.system.core :as event]
    [dragon.event.tag :as tag]
    [starship.blog.reader :as reader]
    [starship.blog.sitemapper :as sitemapper]
    [starship.blog.web.content.page :as page]
    [taoensso.timbre :as log]))

(defn static-routes
  ([system posts]
    (static-routes system posts {}))
  ([system posts routes]
    (merge
      routes
      {"/about/contact.html" (page/contact system posts)
       "/about/disclosure.html" (page/disclosure system posts)
       "/about/index.html" (page/about system posts)
       "/about/license.html" (page/license system posts)
       "/about/powered-by.html" (page/powered-by system posts)
       "/about/privacy.html" (page/privacy system posts)
       "/pages/index.html" (page/pages system posts)
       "/pages/timeline.html" (page/timeline system posts)})))

(defn design-routes
  [system posts routes]
  (merge
    routes
    {"/design/bootstrap-theme.html" (page/bootstrap-theme system posts)
     "/design/example-blog.html" (page/blog-example system posts)
     "/design/example-front-page.html" (page/front-page-example system posts)
     "/design/font-samples.html" (page/font-samples system posts)
     "/design/index.html" (page/design system posts)}))

(defn post-routes
  [system posts routes]
  (merge
    routes
    (blog/get-indexed-archive-routes
      (map vector (iterate inc 0) posts)
      :gen-func (partial page/post system posts)
      :uri-base (config/posts-path system))))

(defn post-listing-routes
  [system posts routes]
  (merge
    routes
    {"/index.html" (page/front-page system posts)
     "/archives/index.html" (page/archives system posts)
     "/categories/index.html" (page/categories system posts)
     "/tags/index.html" (page/tags system posts)
     "/authors/index.html" (page/authors system posts)}))

(defn reader-routes
  [system posts routes]
  (let [route "/atom.xml"]
    (merge
      routes
      {route (reader/atom-feed
               system
               route
               (take (config/feed-count system) posts))})))

(defn sitemaps-routes
  [system routes]
  (let [route "/sitemap.xml"]
    (merge
      routes
      {route (sitemapper/gen system routes)})))

(defn routes
  [system posts]
  (log/trace "Got data:" (pprint (blog/data-for-logs posts)))
  (event/publish system tag/generate-routes-pre)
  (->> (static-routes system posts)
       (design-routes system posts)
       (post-routes system posts)
       (post-listing-routes system posts)
       (reader-routes system posts)
       (sitemaps-routes system)
       (event/publish->> system tag/generate-routes-post)
       vec))

;;; Generator routes

(defn gen-route
  [func msg & args]
  (log/info msg)
  (apply func args))

(def gen-static-routes
  (partial
    gen-route
    static-routes
    "\tGenerating pages for static pages ..."))

(def gen-design-routes
  (partial
    gen-route
    design-routes
    "\tGenerating pages for design pages ..."))

(def gen-post-routes
  (partial
    gen-route
    post-routes
    "\tGenerating pages for blog posts ..."))

(def gen-post-listing-routes
  (partial
    gen-route
    post-listing-routes
    "\tGenerating pages for front page, archives, categories, etc. ..."))

(def gen-reader-routes
  (partial
    gen-route
    reader-routes
    "\tGenerating XML for feeds ..."))

(def gen-sitemaps-routes
  (partial
    gen-route
    sitemaps-routes
    "\tGenerating XML for sitemap ..."))

(defn gen-routes
  [system posts]
  (log/info "Generating routes ...")
  (log/trace "Got data:" (pprint (blog/data-for-logs posts)))
  (event/publish system tag/generate-routes-pre)
  (->> (gen-static-routes system posts)
       (gen-design-routes system posts)
       (gen-post-routes system posts)
       (gen-post-listing-routes system posts)
       (gen-reader-routes system posts)
       (gen-sitemaps-routes system)
       (event/publish->> system tag/generate-routes-post)
       vec))
