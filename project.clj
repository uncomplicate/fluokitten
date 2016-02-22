(defproject uncomplicate/fluokitten "0.4.0-SNAPSHOT"
  :description "Category theory concepts in Clojure - Functors, Applicatives, Monads, Monoids and more."
  :url "https://github.com/uncomplicate/fluokitten"
  :scm {:name "git"
        :url "https://github.com/uncomplicate/fluokitten"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :codox {:src-dir-uri "http://github.com/uncomplicate/fluokitten/blob/master"
          :src-linenum-anchor-prefix "L"
          :namespaces [uncomplicate.fluokitten.core
                       uncomplicate.fluokitten.jvm
                       uncomplicate.fluokitten.protocols
                       uncomplicate.fluokitten.test
                       uncomplicate.fluokitten.utils]
          :exclude [uncomplicate.fluokitten.algo]
          :output-dir "docs/codox"}
  :profiles {:dev {:dependencies [[midje "1.8.3"]
                                  [criterium "0.4.4"]]
                   :global-vars {*warn-on-reflection* true
                                 *unchecked-math* :warn-on-boxed
                                 *print-length* 128}
                   :plugins [[lein-midje "3.1.3"]
                             [codox "0.9.0"]]}})
