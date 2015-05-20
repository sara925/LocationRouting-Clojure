(ns clojure-training.core
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:gen-class))

(def nodeMaps [{}])


(defn initNode 
"inizializza la struttura dati con i dati del benchmark"
 [seq]
 (def nodes (zipmap [:id :x :y] [(first seq) (second seq) (last seq)]))
 (def nodeMaps (conj nodeMaps nodes))
)

(defn parse
"Parsing benchmark"
  [lines]
  (doseq [line lines] ;;itero su tutte le stringhe
    (let [lineSplit (take 3 (str/split  line #"\s"))] 
         
            (if (number? (read-string (first lineSplit)))
              (initNode lineSplit) 
            )
                    
    ) 
  )
) 
  

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  
  (with-open [rdr (io/reader "./src/clojure_training/lu980.txt")]
    (parse (line-seq rdr)))
  (println  nodeMaps)
)





  
