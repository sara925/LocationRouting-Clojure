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

(defn find-border-customers 
  []
  (def borders (update-in borders [:xmin] + (:x (first (sort-by :x  clients)))))
  (def borders (update-in borders [:ymin] + (:y (first (sort-by :y clients)))))
  (def borders (update-in borders [:xmax] + (:x (last (sort-by :x clients)))))
  (def borders (update-in borders [:ymax] + (:y (last (sort-by :y clients)))))
)

(defn ediff
 [x1 x2]
 (math/expt (- x1 x2) 2)
)

(defn computeCost 
  [map1 map2]
  (math/sqrt (+ (ediff (:x map1) (:x map2)) (ediff (:y map1) (:y map2))))
)

(defn compSetCost
  [s1]
  (def array [])
  ;;stimo il costo del ciclo hamiltoniano come la somma delle distanze dirette dal magazzino ai nodi associati
  (let [storeNum (some #(if (number? %) %) s1 )]
    (doseq [cl s1] (and (map? cl) (def array (conj array (computeCost  cl (get stores storeNum))))))
    (+ (reduce + array) (:build (get stores storeNum)))
  )
)

(defn unionCollection
  [aset]
  (def un {})
  (doseq [x aset] (def un (set/union un  x)))
  un
)


(defn find-radius
  [store]
  
  (def radius)
  (let [Xs (:x store) Ys (:y store) Xmin (:xmin borders) Xmax (:xmax borders) Ymin (:ymin borders) Ymax (:ymax borders)]
    
    (if (> (math/abs (- Xs Xmin))  (math/abs (- Xs Xmax))) 
      (def maxX Xmin)
      (def maxX Xmax))

     (if (> (math/abs (- Ys Ymin))  (math/abs (- Ys Ymax))) 
      (def maxY Ymin)
      (def maxY Ymax))

   ; (first (apply min-key second (map-indexed vector [1 2 4 0 5])))
   ; (def maxX (max (math/abs (- Xs Xmin)) (math/abs (- Xs Xmax))))
    ;(def maxY (max (math/abs (- Ys Ymin)) (math/abs (- Ys Ymax))))
    (math/sqrt (+ (ediff maxX Xs) (ediff maxY Ys)))
  )
)

(defn initSubSetArray
  [store]
  (println "\nraggio max "(find-radius store))
  
  (def r (/ (find-radius store) 4))

  (def slots [[] [] [] []])
  (def sumSlotDist [0 0 0 0])
  
  ;assegno ogni cliente ad una fascia
  (doseq [cl clients]
    (let [ dist (computeCost cl store) slot (quot (computeCost cl store)  r)]
      ;;(println cl)
      ;(println (/ (computeCost cl store) r))
      ;;(println slot)
      (def slots (assoc slots (int slot) (conj (get slots (int slot)) 
 {:probability 0 :dist dist :cl cl})))
      (def sumSlotDist (update-in sumSlotDist[(int slot)] + dist))
    ))
  (println r)
  (println "num slots: "(count slots))
 ;; (println (first slots))
  ;;print of probabilty per slots
  (println "Sums of probability per slot" sumSlotDist)
  ;assegno le probabilità ai clienti
  


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

  (println "Massima capacità: "storeCapacity "\nMassima domanda: "maxDemand)
  (println "Numero magazzini tot: "numPossMag)
  
  ;; (println (set  stores))


  (find-border-customers)
  (initSubSetArray (first stores))
  
)
