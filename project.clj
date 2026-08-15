(defproject net.clojars.frknzd/re-frame.vertica "0.3.7"
  :description "An in-app vertical slice through re-frame data flow"
  :url "https://github.com/frknzd/re-frame.vertica"
  :license {:name "MIT"
            :url "https://github.com/frknzd/re-frame.vertica/blob/main/LICENSE"}
  :scm {:name "git"
        :url "https://github.com/frknzd/re-frame.vertica"
        :connection "scm:git:https://github.com/frknzd/re-frame.vertica.git"
        :developerConnection "scm:git:ssh://git@github.com/frknzd/re-frame.vertica.git"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/clojurescript "1.12.42"]
                 [re-frame/re-frame "1.4.7"]
                 [reagent/reagent "1.2.0"]
                 [com.cognitect/transit-cljs "0.8.280"]]
  :source-paths ["src"]
  :resource-paths ["resources"]
  :test-paths ["test"]
  :deploy-repositories
  [["clojars" {:url "https://repo.clojars.org"
                :username :env/clojars_username
                :password :env/clojars_password
                :sign-releases false}]])
