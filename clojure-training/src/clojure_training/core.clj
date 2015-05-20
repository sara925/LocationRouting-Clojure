(ns clojure-training.core
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:gen-class))


(defn parse
"Parsing bnchmark"
  [lines]
  (doseq [line lines](println (str/split  line #"\s")))  
  

  (defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (with-open [rdr (io/reader "./src/clojure_training/lu980.txt")]
    (parse (line-seq rdr)))

  ))


  
