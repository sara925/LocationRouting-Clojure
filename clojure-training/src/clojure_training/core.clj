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
<<<<<<< HEAD
  (with-open [rdr (io/reader "/home/utente/lu980.txt")]
    (printf "%s\n" (clojure.string/join "n-" (line-seq rdr))))
)
=======
  (with-open [rdr (io/reader "./src/clojure_training/lu980.txt")]
    (parse (line-seq rdr)))

  ))
>>>>>>> f7665470520dd046f99342ec51e523130565207b


  
