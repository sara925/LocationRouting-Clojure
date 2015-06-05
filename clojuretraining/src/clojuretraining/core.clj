(ns clojuretraining.core
	(:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:require [clojure.math.numeric-tower :as math])

  ;(:use [clojuretraining.instanceinit :as myinit])
  ;(:require [clojuretraining.instanceinit :as myinit])
)

(load "instanceinit")
(load "subsetinit")
(load "kruskall")
 
;;---general purpose data structures--
(def nodeMaps [])   ;array of maps, each map represents a node read from a TSP benchmark
(def maxDemand)     ;every customer's demand varies from 1 to maxDemand (default: [1,20])
(def numPossMag)    ;number of possible storehouse locations 
(def storeCapacity) ;number representing the maximum storehouse capacity, it is the same for every storehouse
(def stores [])     ;an array of numPossMag nodes randomly choosen in nodeMaps
(def clients [])    ;array of customers locations obtained as nodeMaps - stores

;;---greedy function and GRASP data structures--
(def subSetArray []);contains a random number of randomly choosen subsets of clients, every subsets represents the binding between a storehouse and its customers

(def borders {:xmin 0 :xmax 0 :ymin 0 :ymax 0})
(def slotsProbability (reductions + [50 30 15 5]))  ;probabilità delle varie fasce

;;----------------------------------------------

(defn createSubSet 
 [slots, subSet]
 
 (def tmp subSet) (def freeSpace storeCapacity)
 (def slotsCum (createCumulativeSlots slots));lista di liste cumulative

 (loop [iter 0]
   (let [idx (fish slotsProbability)] 

     (if (not (empty? (get slots idx)))
       
       (do 
         (let [idx2 (fish (nth slotsCum idx)) cl (:cl (get (get slots idx) idx2))] 
              (if (not (contains? tmp cl)) 
                (do (def tmp (set/union tmp (into #{} (vector cl))))
                    (def freeSpace (- freeSpace (:capacity cl)))))
              ))))

   (if (> freeSpace maxDemand) 
     (recur (inc iter)))
 )

 tmp
)

(defn find-best-store
  [cl]
  ;trovo il magazzino a distanza minore da cl, ritorno costo e nodo magazzino in un array 
  (apply min-key #(first %) (map #(vector (computeCost cl %) %) stores)))


(defn assign-to-set
  [cl, set]

  (find-best-store cl)
)

;;get the set in the compisite map array
(defn getSet
  [cset]
  ;;return only the set map in the cset composite map
  (get-in cset [:set] ) 
)

;;get the all set in the subSetArray
(defn getAllSet
  [setArray]
  ;;all the store map in the subSetArray
  (map (fn [x] (get-in x [:set]) ) setArray)

)


(defn initSubSetArray
 []

  (find-border-customers)
  (doseq [iter (range (count stores))] 
 
    (def slots (assignProbability (get stores iter)))
    (doseq [x (range 3)]
      ;;costruzione del subset come insieme di mappe clienti e mappa store
      ;;(def subSet #{ {:store (get stores iter)} {:x 1 :y 2} }) ;TODO aggiungi il set
      (def subSet #{ (get stores iter)})
      (def subSet (createSubSet slots subSet))
      ;;modifica del subset nella struttura
      ;{: store nodo_magazzino :set insieme dei nodi del set compreso nodo maggazino}
      (def subSet {:store (get stores iter) :set subSet})
      (def subSetArray (conj subSetArray subSet)))
        
  )
  ;;(println (first subSetArray))
  ;;la struttura è quella voluta
  ;;definisco una function per utilizzare solo la mappa set(clienti+store nelle varie funzioni)
  (def notAssigned (set/difference (set clients) (set/intersection  (set clients) (reduce set/union  (getAllSet subSetArray) ))))
  (if (not (empty? notAssigned))
    ()  
  )

)


;;------------------ MAIN -----------
(defn -main
  "Location routing"
  [& args]
  (if (or (empty? args) 
          (> (count args) 1))
    ;;set to default values
    (do (def storeCapacity 600 )
        (def maxDemand 20)) 
    ;;read values from input file
	(read-param-file (str "./resources/"(first args))))

  ;;apro il file e lo leggo una riga alla volta, lo passo poi al parser
  ;; per l'inizializzazione della struttura dati che conterrà tutti i nodi
  (read-benchmark-file "lu980.txt")
  
  ;;instance initialization
  (instance-init)
  
  ;;GRASP procedure loop
  (loop [iter 1]
    ;;construction of a greedy solution as a starting point
    

    ;;local search proceduere
    ;;...TODO..

    (if (< iter 1)
      (recur (inc iter)))
    )

  (println "Massima capacità: "storeCapacity " Massima domanda: "maxDemand)
  (println "Numero magazzini tot: "numPossMag)
  

   (initSubSetArray)
   ;;to test the changes
   ;;we will find the MST of the first subSet
   (MST (first subSetArray))
   ;;funziona valutare se queste modifiche ci piacciono o meno
)
