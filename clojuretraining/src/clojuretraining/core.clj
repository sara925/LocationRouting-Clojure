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
(def slotsProbability (reductions + [50 30 15 5]))  ;probabilità delle varie fasce

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

(defn assignProbability
  [store]
  
  (def r (/ (find-radius store) 4))

  (def slots [[] [] [] []])
  (def sumSlotDist [r (* 2 r) (* 3 r) (* 4 r)])
  
  ;assegno ogni cliente ad una fascia
  (doseq [cl clients]
    (let [ dist (computeCost cl store) slot (quot (computeCost cl store)  r)]

      (def slots (assoc slots (int slot) (conj (get slots (int slot)) {:probability 0 :dist dist :cl cl})))
     ; (def sumSlotDist (update-in sumSlotDist[(int slot)] + dist))

    ))
 
  ;assegno le probabilità ai clienti
  (doseq [i (range 4)]
    (let [sumDist (get sumSlotDist i) slot_i (get slots i)]
      (doseq [j (range (count slot_i))]
        
        (let [ probClj (/ (- sumDist (:dist (get slot_i j))) sumDist)]
          (def slots (assoc slots i (assoc-in (get slots i) [j :probability] probClj ))))
  )))

  slots
)

(defn fish
  [coll]
  
  (def randomVal (rand (last  coll)))
  (let [selected (apply min (filter #(> % randomVal) coll))]
    
  (.indexOf coll selected))
)

(defn createCumulativeSlots
  [slots]

  (doall (map #(reductions +  (map :probability %)) slots))
)

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


(defn initSubSetArray
 []

  (find-border-customers)

   

  (doseq [iter (range (count stores))] 
 
    (def slots (assignProbability (get stores iter)))
    
    (doseq [x (range 3)]
      (def subSet #{ iter })
      (def subSet (createSubSet slots subSet))
      (def subSetArray (conj subSetArray subSet)))
        
  )

  
  (def notAssigned (set/difference (set clients) (set/intersection  (set clients) (reduce set/union  subSetArray ))))
  (if (not (empty? notAssigned))

    (def {:idM 0 :minD Long/MAX_VALUE})
    (doseq [c notAssigned]
       (doseq [m stores]
           (computeCost c m ))
    ))




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

)
