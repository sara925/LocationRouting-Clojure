(ns clojure-training.core
  (:require [clojure.java.io :as io])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (with-open [rdr (io/reader "/home/utente/lu980.txt")]
    (printf "%s\n" (clojure.string/join "n-" (line-seq rdr))))
)


