(ns starship.blog.web.content.data
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string]
    [dragon.blog.categories :as blog-categories]
    [dragon.blog.content.block :as block]
    [dragon.blog.content.data :as page-data]
    [dragon.blog.core :as blog]
    [dragon.blog.tags :as blog-tags]
    [dragon.config.core :as config]
    [markdown.core :as markdown]
    [taoensso.timbre :as log]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Constants & Helper Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn legal-block-names
  [system]
  (block/legal-block-names
    (config/blocks-enabled system)))

(defn get-category-theme
  [opts]
  (let [cat-key (:category-key opts)]
    (case cat-key
      :design "default"
      cat-key (name cat-key)
      "default")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Base Data Functions   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn common
  ([system]
    (common system {}))
  ([system posts]
    (common system posts {}))
  ([system posts additional-opts]
    (let [base-opts {:site-title (config/name system)
                     :site-description (config/description system)}
          opts (merge base-opts additional-opts)
          category-theme (get-category-theme opts)]
      (page-data/common
        posts
        (assoc opts :category-theme category-theme)))))

(defn about-opts
  [opts]
  (page-data/default-markdown-content-opts
    (assoc opts :category-key :about)))

(defn archive-opts
  [opts]
  (page-data/default-data-content-opts
    (assoc opts :title "Archives"
                :category-key :archives
                :category-description (str "All previous posts are listed "
                                           "here by year and month."))))

(defn pages-opts
  [opts]
  (page-data/default-markdown-content-opts
    (assoc opts :category-key :pages
                :category-description (str "Some of the content on the blog "
                                           "is maintained as actual pages "
                                           "rather than as posts. You will "
                                           "find links to them here, "
                                           "below.")
                :pages "pages")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Static Pages Data   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn about
  [system posts]
  (common system
          posts
          (about-opts
            {:title "About"
             :content-filename "about.md"})))

(defn community
  [system posts]
  (let [data-content {}]
    (common system
            posts
            (page-data/default-data-content-opts
              {:title "Community"
               :category-key :community}))))

(defn contact
  [system posts]
  (common system
          posts
          (about-opts
            {:title "Contact Us"
             :content-filename "contact.md"})))

(defn disclosure
  [system posts]
  (common system
          posts
          (about-opts
            {:title "Disclosure Policy"
             :content-filename "disclosure.md"})))

(defn license
  [system posts]
  (common system
          posts
          (about-opts
            {:title "Content License"
             :content-filename "license.md"})))

(defn pages
  [system posts]
  (common system
          posts
          (pages-opts
            {:title "Pages"
             :content-filename "pages.md"})))

(defn powered-by
  [system posts]
  (common system
          posts
          (about-opts
            {:title "Powered By"
             :content-filename "powered-by.md"})))

(defn privacy
  [system posts]
  (common system
          posts
          (about-opts
            {:title "Privacy Policy"
             :content-filename "privacy.md"})))

(defn timeline
  [system posts]
  (common system
          posts
          (pages-opts
            {:title "A Timeline of Starship Programs"
             :content-filename "timeline.md"})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Dynamic Pages Data   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn post
  [system posts post-data]
  (common system
          posts
          (archive-opts
            {:post-data post-data
             :blocks (block/get-blocks (legal-block-names system) post-data)
             :tags (blog-tags/unique [post-data])})))

(defn front-page
  [system all-posts top-posts &
   {:keys [above-fold-count below-fold-count column-count]}]
  (let [above-posts (take above-fold-count top-posts)
        headliner (first above-posts)
        grouped-posts (partition column-count
                                 (nthrest above-posts 1))
        below-posts (nthrest top-posts above-fold-count)]
    (common system
            all-posts
            {:title "Starship Tools"
             :category-key :home
             :category-description (str "A blog for sharing the open source "
                                        "code, tools, and ideas that will "
                                        "allow us to traverse interstellar "
                                        "distances. Focus is on control "
                                        "systems, distributed fault-tolerance, "
                                        "high-concurrency, and soft "
                                        "real-time operations.")
             :tags (blog-tags/get-stats all-posts)
             :categories (blog-categories/get-stats all-posts)
             :headliner headliner
             :posts-data grouped-posts
             :posts-count (count top-posts)
             :above-count (count above-posts)
             :below-count (count below-posts)
             :below-fold-data below-posts})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Listings Pages   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn archives
  [system posts]
  (common system
          posts
          (archive-opts
            {:posts-data (blog/group-data :archives posts)})))

(defn categories
  [system posts]
  (common system
          posts
          (page-data/default-data-content-opts
            {:title "Categories"
             :category-key :categories
             :category-description (str "All previous posts are listed here "
                                        "by primary topic.")
             :posts-data (blog/group-data :categories posts)})))

(defn tags
  [system posts]
  (common system
          posts
          (page-data/default-data-content-opts
            {:title "Tags"
             :category-key :tags
             :category-description (str "All previous posts are listed here "
                                        "under all the tags they have. ")
             :posts-data (blog/group-data :tags posts)})))

(defn authors
  [system posts]
  (common system
          posts
          (page-data/default-data-content-opts
            {:title "Authors"
             :category-key :authors
             :category-description (str "All previous posts are listed here "
                                        "by author.")
             :posts-data (blog/group-data :authors posts)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Design Pages Data   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn design
  [system posts]
  (common system
          posts
          (page-data/default-data-content-opts
            {:title "Design"
             :category-key :design
             :category-description "Exploring look and feel ..."})))
