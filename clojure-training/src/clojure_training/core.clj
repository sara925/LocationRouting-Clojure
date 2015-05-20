(ns clojure-training.core
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:gen-class))



(def nodeMaps [])
(def numPossMag)
(def storeCapacity 600)





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
;;------------------------------------------- 

;;-----------FUNZIONI INIZIALIZZAZIONE ISTANZA
(defn instanceInit
  " "
  []
  ;; determino una stima del numero minimo di magazzini necessari
  ;;((numero clienti * max richiesta cliente)/ capacitàMag) + rand(0,20)
  (def numPossMag (+ (rand-int 20)   
                     (quot (* (count nodeMaps) 20) storeCapacity))) 
  
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




;;-----------------------------------
(defn -main
  "Carica i dati dal benchmark e risolve il problema di location routing"
  [& args]
  ;;apro il file e lo leggo una riga alla volta, lo passo poi al parser
  ;; per l'inizializzazione della struttura dati che conterrà tutti i nodi
  (with-open [rdr (io/reader "./src/clojure_training/lu980.txt")]
    (parse (line-seq rdr)))
  
  ;;seleziono i possibili nodi magazzino e assegno i pesi
  (instanceInit)

  (println "Numero magazzini tot:" numPossMag)
  (println (first nodeMaps))
)





  
