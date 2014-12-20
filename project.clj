(defproject uncomplicate/fluokitten "0.3.1-SNAPSHOT"
  :description "Category theory concepts in Clojure - Functors, Applicatives, Monads, Monoids and more."
  :url "https://github.com/uncomplicate/fluokitten"
  :scm {:name "git"
        :url "https://github.com/uncomplicate/fluokitten"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0-alpha2"]]
  :codox {:src-dir-uri "http://github.com/uncomplicate/fluokitten/blob/master"
          :src-linenum-anchor-prefix "L"
          :exclude [uncomplicate.fluokitten.algo]
          :output-dir "docs/codox"}
  :profiles {:dev {:dependencies [[midje "1.6.3"]]
                   :plugins [[lein-midje "3.1.3"]
                             [codox "0.8.10"]
                             [lein-marginalia "0.8.0"]]}})
