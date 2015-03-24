(defproject algos "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]]

  :resource-paths ["src/resource", "test/resource"]
  
  :dependencies [[org.clojure/clojure "1.7.0-alpha5"]
                 [clojurewerkz/buffy "1.0.0"]
                 [org.clojure/tools.nrepl "0.2.7"]])
