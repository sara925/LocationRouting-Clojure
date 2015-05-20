(ns clojure-training.core
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:gen-class))



(def nodeMaps [])
(def numPossMag)
(def storeCapacity)
(def maxDemand)





;;-----------FUNZIONI PARSING-----------------
(defn initNode 
  "inizializza la struttura dati con i dati del benchmark"
  [seq]
  (def nodes (zipmap [:id :x :y :capacity] [(first seq) (second seq) (last seq) 0]))
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

(defn parseParam
  "Parameters parsing from input file"
  [seq]
  ;;iterate over the file per line
  (doseq [line seq]
    (let [lineSplit (take 2 (str/split line #":"))]
      (if (= (str/trim(first lineSplit)) (str/trim "STORE CAPACITY"))
        (def storeCapacity (read-string (second lineSplit))))
      (if (= (str/trim(first lineSplit)) (str/trim "MAXIMUM DEMAND"))
        (def maxDemand (read-string (second lineSplit))))
    )
  )
)
;;------------------------------------------- 

;;-----------FUNZIONI INIZIALIZZAZIONE ISTANZA

(defn instanceInit
  " "
  []
  ;; determino una stima del numero minimo di magazzini necessari
  ;;((numero clienti * max richiesta cliente)/ capacitàMag) + rand(0,20)
  (def numPossMag (+ (rand-int 20)   
                     (quot (* (count nodeMaps) maxDemand) storeCapacity))) 
  
  ;;assegno al primo nodo capacità Q= storeCapacity
  (loop [iter 0]
    (let [n 0]
      (def nodeMaps (update-in nodeMaps [n :capacity] + storeCapacity))
      )
      (if (< iter numPossMag)
        (recur (inc iter))
      )
  )
)


;;------------------ MAIN -----------
(defn -main
  "Carica i dati dal benchmark e risolve il problema di location routing"
  [& args]
  (if (or (empty? args) 
          (> (count args) 1))
      ;;set to default values
      (do (def storeCapacity 600 )
          (def maxDemand 20)) 
      ;;read values from input file
      (with-open [rdr (io/reader (str "./resources/"(first args)))]
        (parseParam (line-seq rdr)))
  )

  ;;apro il file e lo leggo una riga alla volta, lo passo poi al parser
  ;; per l'inizializzazione della struttura dati che conterrà tutti i nodi
  (with-open [rdr (io/reader "./src/clojure_training/lu980.txt")]
    (parse (line-seq rdr)))
  
  ;;seleziono i possibili nodi magazzino e assegno i pesi
  (instanceInit)

  (println "Massima capacità: " storeCapacity "\nMassima domanda: "maxDemand)

  (println "Numero magazzini tot:" numPossMag)
  (println (first nodeMaps))
)





  
