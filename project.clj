(defproject
  boot-project
  "0.0.0-SNAPSHOT"
  :dependencies
  [[adzerk/boot-cljs "1.7.228-1" :scope "test"]
   [adzerk/boot-cljs-repl "0.3.3" :scope "test"]
   [adzerk/boot-reload "0.4.12" :scope "test"]
   [pandeiro/boot-http "0.7.3" :scope "test"]
   [com.cemerick/piggieback "0.2.1" :scope "test"]
   [org.clojure/tools.nrepl "0.2.12" :scope "test"]
   [weasel "0.7.0" :scope "test"]
   [org.clojure/clojure "1.9.0-alpha15"]
   [org.clojure/clojurescript "1.9.229"]
   [org.clojure/test.check "0.9.0"]
   [prismatic/plumbing "0.5.3"]
   [rum "0.10.4"]
   [aysylu/loom "1.0.0"]]
  :repositories
  [["clojars" {:url "https://clojars.org/repo/"}]
   ["maven-central" {:url "https://repo1.maven.org/maven2"}]]
  :source-paths
  ["src/cljs" "resources"])