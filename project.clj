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
  :dependencies []
  :source-paths ["src"]
  :profiles {
    :dragon {
      :domain "starship.tools"
      :name "Starship Tools Blog"
      :description "A Blog to Explore Really, Really Cool Stuff. Code Suff. In Space."
      :port 5099
      :output-dir "blog"
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
    :base {
      :plugins [
        [lein-shell "0.5.0"]]}
    :dev {
      :source-paths ["dev-resources/src"]
      :main starship.blog.main
      :dependencies [
        [clojusc/trifl "0.3.0-SNAPSHOT"]
        [dragon "0.5.0-SNAPSHOT"]
        [http-kit "2.2.0"]
        [leiningen-core "2.7.1"]
        [org.clojure/clojure "1.8.0"]
        [org.clojure/data.generators "0.1.2"]
        [org.clojure/data.xml "0.0.8"]
        [org.clojure/math.combinatorics "0.1.4"]
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
    ;; Development aliases
    "repl"
      ^{:doc "A custom blog REPL that overrides the default one"}
      ["with-profile" "+test,+custom-repl,+cli" "repl"]
    "web"
      ^{:doc "Run a local web service for the blog"}
      ["with-profile" "+test,+cli" "run" "-m" "starship.blog.main" ":run"]
    ;; Content-genereated (and related) aliases
    "setup-sass"
      ["with-profile" "base" "shell" "dev-resources/scripts/setup-sass"]
    "clean-output"
      ["with-profile" "base" "shell" "dev-resources/scripts/clean-output"]
    "gen-html"
      ["with-profile" "base" "shell" "dev-resources/scripts/copy-html"]
    "gen-assets"
      ["with-profile" "base" "shell" "dev-resources/scripts/copy-assets"]
    "gen-css"
      ["with-profile" "base" "shell" "dev-resources/scripts/regen-css"]
    "gen-blog"
      ^{:doc "Generate static content for the blog"}
      ["trampoline" "with-profile" "+test,+cli" 
       "run" "-m" "starship.blog.main" "gen"]
    "gen-all"
      ["do"
        ["clean-output"]
        ["gen-html"]
        ["gen-assets"]
        ["gen-css"]
        ["clean"]
        ["gen-blog"]]
    ;; Content-publishing (and related) aliases
    "commit-regen"
      ["with-profile" "base" "shell" "dev-resources/scripts/commit-regen"]
    "sync-aws"
      ["with-profile" "base" "shell" "dev-resources/scripts/sync-aws"]
    "publish-all-aws"
      ["with-profile" "base" "shell" "dev-resources/scripts/publish-all-aws"]
    "publish-committed-aws"
      ["with-profile" "base" "shell" "dev-resources/scripts/publish-committed-aws"]
    "publish-modified-aws"
      ["with-profile" "base" "shell" "dev-resources/scripts/publish-modified-aws"]
    "publish"
      ["with-profile" "base" "do"
        ["gen-all"]
        ["commit-regen"]
        ["sync-aws"]]
    ;; Dependency checks, linting, and tests
    "check-vers" ["with-profile" "+test" "ancient" "check" ":all"]
    "check-jars" ["with-profile" "+test" "do"
      ["deps" ":tree"]
      ["deps" ":plugin-tree"]]
    "check-deps" ["do"
      ["check-jars"]
      ["check-vers"]]
    "kibit" ["with-profile" "+lint" "kibit"]
    "eastwood" ["with-profile" "+lint" "eastwood" "{:namespaces [:source-paths]}"]
    "lint" ["do"
      ["kibit"]
      ;["eastwood"]
      ]
    "ubercompile" ["with-profile" "+ubercompile,+test,+cli" "compile"]
    "ltest" ["with-profile" "+test" "ltest"]    
    "build"
      ^{:doc "Perform build tasks for CI/CD & releases\n\n Additional aliases:"}
      ["do"
        ;["check-deps"]
        ["lint"]
        ["docs"]
        ["ubercompile"]
        ["clean"]
        ["uberjar"]
        ["clean"]
        ["test"]]})
