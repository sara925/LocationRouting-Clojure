(defproject clojuretraining "0.1.0-SNAPSHOT"
  :description "FIXME:description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
			  [org.clojure/math.numeric-tower "0.0.4"]
                          [org.jordanlewis/data.union-find "0.1.0"]]
  :main ^:skip-aot clojuretraining.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
