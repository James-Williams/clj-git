(defproject clj-git "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [clj-message-digest "1.0.0"]]
  :main ^:skip-aot clj-git.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
