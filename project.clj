;;   Copyright (c) Dragan Djuric. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) or later
;;   which can be found in the file LICENSE at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(defproject uncomplicate/fluokitten "0.8.0-SNAPSHOT"
  :description "Category theory concepts in Clojure - Functors, Applicatives, Monads, Monoids and more."
  :url "https://github.com/uncomplicate/fluokitten"
  :scm {:name "git"
        :url "https://github.com/uncomplicate/fluokitten"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :codox {:src-dir-uri "http://github.com/uncomplicate/fluokitten/blob/master"
          :src-linenum-anchor-prefix "L"
          :namespaces [uncomplicate.fluokitten.core
                       uncomplicate.fluokitten.jvm
                       uncomplicate.fluokitten.protocols
                       uncomplicate.fluokitten.test
                       uncomplicate.fluokitten.utils]
          :exclude [uncomplicate.fluokitten.algo]
          :output-path "docs/codox"}

  :profiles {:dev {:dependencies [[midje "1.9.1"]
                                  [criterium "0.4.4"]]
                   :global-vars {*warn-on-reflection* true
                                 *print-length* 128}
                   :plugins [[lein-midje "3.2.1"]
                             [lein-codox "0.10.3"]]
                   :jvm-opts ^:replace [#_"--add-opens=java.base/jdk.internal.ref=ALL-UNNAMED"]}}

  :javac-options ["-target" "1.8" "-source" "1.8" "-Xlint:-options"])
