(defproject blorg "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[clj-time "0.9.0"]
                 [environ "1.0.0"]
                 [garden "1.2.5"]
                 [hiccup "1.0.5"]
                 [instaparse "1.3.5"]
                 [org.clojure/clojure "1.6.0"]
                 [watchtower "0.1.1"]]
  :profiles {:dev {:dependencies [[speclj "3.2.0"]]}}
  :plugins [[speclj "3.2.0"]]
  :aliases {"autotest" ["spec" "-a"]
            "autoquiet" ["spec" "--format=progress" "-r" "v"]}
  :test-paths ["spec"]
  :main blorg.core)
