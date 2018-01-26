(defn get-banner
  []
  (str
    (slurp "resources/text/banner.txt")
    (slurp "resources/text/loading.txt")))

(defn get-prompt
  [ns]
  (str "\u001B[35m[\u001B[34m"
       ns
       "\u001B[35m]\u001B[33m λ\u001B[m=> "))

(defproject starship/tools-blog "2.0.0-SNAPSHOT"
  :description "Blog for Starship Tools"
  :url "https://starship.tools/"
  :scm {
    :name "git"
    :url "https://github.com/starship-tools/blog"}
  :license {
    :name "Apache License, Version 2.0"
    :url "http://www.apache.org/licenses/LICENSE-2.0"}
  :exclusions [
    [org.clojure/clojure]
    [org.clojure/clojurescript]]
  :dependencies [
    [clojusc/trifl "0.3.0-SNAPSHOT"]
    [dragon "0.5.0-SNAPSHOT"]
    [org.clojure/clojure "1.8.0"]
    [org.clojure/data.generators "0.1.2"]
    [org.clojure/data.xml "0.0.8"]
    [org.clojure/math.combinatorics "0.1.4"]]
  :source-paths ["src"]
  :profiles {
    :dragon {
      :domain "starship.tools"
      :name "Starship Tools Blog"
      :description "A Blog to Explore Really, Really Cool Stuff. Code Suff. In Space."
      :port 5099
      :output-dir "docs"
      :base-path "/"
      :posts-path "/archives"
      :posts-path-src "./posts"
      :feed-count 20
      :blocks {
        :enabled #{
          "article-body-ads"
          "article-sidebar-comments-links"}}
      :robots {
        :disallow #{
          "/data/"
          "/design/"}}
      :cli {
        :log-level :debug
        :log-nss [starship.blog dragon]}
      :workflow {
       :storage :db}
      :apis {
        :flickr {
          :access "~/.flickr/starship-tools/access.key"}
        :twitter {
          :app-consumer {
            :key "~/.twitter/starship-tools/app-consumer.key"
            :secret "~/.twitter/starship-tools/app-consumer.secret"}
          :user-access {
            :token "~/.twitter/starship-tools/user-access.token"
            :secret "~/.twitter/starship-tools/user-access.secret"}}}}
    :ubercompile {:aot :all}
    :custom-repl {
      :repl-options {
        :init-ns starship.blog.dev
        :prompt ~get-prompt
        init ~(println (get-banner))}}
    :dev {
      :source-paths ["dev-resources/src"]
      :main starship.blog.main
      :plugins [
        [lein-shell "0.5.0"]
        [lein-simpleton "1.3.0"]]
      :dependencies [
        [http-kit "2.2.0"]
        [leiningen-core "2.7.1"]
        [org.clojure/tools.namespace "0.2.11"]]}
    :test {
      :dependencies [
        [clojusc/ltest "0.3.0-SNAPSHOT"]]
      :plugins [
        [jonase/eastwood "0.2.4"]
        [lein-ancient "0.6.12"]
        [lein-bikeshed "0.4.1" :exclusions [org.clojure/tools.namespace]]
        [lein-kibit "0.1.5"]
        [lein-ltest "0.3.0-SNAPSHOT"]
        [venantius/yagni "0.1.4"]]}
    :cli {
      :resource-paths ["posts"]
      :exclusions [
        clj-http
        clojusc/cljs-tools
        common-codec
        commons-logging
        joda-time
        org.apache.maven.wagon/wagon-http
        org.clojure/clojurescript
        org.clojure/clojure]
      :dependencies [
        [clj-http "2.0.1"]
        [clojusc/cljs-tools "0.2.0-SNAPSHOT"]
        [com.draines/postal "2.0.2"]
        [com.google.api-client/google-api-client "1.22.0"]
        [com.google.apis/google-api-services-plusDomains "v1-rev434-1.22.0"]
        [commons-codec "1.10"]
        [commons-logging "1.2"]
        [joda-time "2.9.9"]
        [org.apache.maven.wagon/wagon-http "2.10"]
        [org.clojure/data.json "0.2.6"]
        [twitter-api "1.8.0"]]}}
  :aliases {
    "repl"
      ^{:doc "A custom blog REPL that overrides the default one"}
      ["with-profile" "+test,+custom-repl,+cli" "repl"]
    "setup-sass"
      ["shell" "dev-resources/scripts/setup-sass"]
    "clean-docs"
      ["shell" "dev-resources/scripts/clean-docs"]
    "gen-html"
      ["shell" "dev-resources/scripts/copy-html"]
    "gen-assets"
      ["shell" "dev-resources/scripts/copy-assets"]
    "gen-css"
      ["shell" "dev-resources/scripts/regen-css"]
    "gen-all"
      ["do"
        ["clean-docs"]
        ["gen-html"]
        ["gen-assets"]
        ["gen-css"]
        ;["gen-blog"]
        ]
    "commit-regen"
      ["shell" "dev-resources/scripts/commit-regen"]
    "sync-aws"
      ["shell" "dev-resources/scripts/sync-aws"]
    "publish-all-aws"
      ["shell" "dev-resources/scripts/publish-all-aws"]
    "publish-modified-aws"
      ["shell" "dev-resources/scripts/publish-modified-aws"]
    "publish"
      ["do"
        ["gen-all"]
        ["commit-regen"]
        ["sync-aws"]]
    "check-deps"
      ^{:doc "Check if any deps have out-of-date versions"}
      ["with-profile" "+test" "ancient" "check" ":all"]
    "lint"
      ^{:doc "Perform lint checking"}
      ["with-profile" "+test" "kibit"]
    "ltest"
      ["with-profile" "+test" "ltest"]
    "blog"
      ^{:doc "The bog CLI; type `lein blog help` or `blog help` for commands"}
      ["with-profile" "+cli"
       "run" "-m" "starship.blog.main" "cli"]
    "gen"
      ^{:doc "Generate static content for the blog"}
      ["run" "-m" "starship.blog.core/generate"]
    "web"
      ^{:doc "Run a local web service for the blog"}
      ["run" "-m" "starship.blog.core/web"]
    "dev"
      ^{:doc "Generate blog content and run local web service"}
      ["run" "-m" "starship.blog.core/log+generate+web"]
    "ubercompile" ["with-profile" "+ubercompile,+test,+cli" "compile"]
    "build"
      ^{:doc "Perform build tasks for CI/CD & releases\n\n Additional aliases:"}
      ["with-profile" "+test,+cli" "do"
        ;["check-deps"]
        ;["lint"]
        ["test"]
        ["compile"]
        ["uberjar"]]})
