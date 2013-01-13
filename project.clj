(defproject redcat "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.0-RC2"]]
  :profiles {:dev {:dependencies [[midje "1.5-alpha4"]
                                  [org.clojure/data.generators "0.1.0"]
                                  [lein2-eclipse "2.0.0"]]
                   :plugins [[lein-midje "2.0.4"]]}})
