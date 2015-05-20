(defproject clojure-training "0.1.0-SNAPSHOT"
  :description "A set of T nodes is given: N of them represent clients locations and M of them represent instead possible storehouse location.
Every client demand is supplied by only one storehouse. The aim is to choose where to build the storehouses among the M possible location, reducing both the construction cost and the supply cost. The supply cost is computed for every storehouse as the cost of the minimum 
hamiltonian cycle connecting all clients supplied by the storehouse."
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot clojure-training.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
