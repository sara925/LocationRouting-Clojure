(ns clojuretraining.core
	(:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:require [clojure.data :as data])
  (:require [clojure.math.numeric-tower :as math])

  ;(:use [clojuretraining.instanceinit :as myinit])
  ;(:require [clojuretraining.instanceinit :as myinit])
)

(load "instanceinit")
(load "subsetinit")
(load "kruskall")
(load "two-opt")
(load "pgraph")
 
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

(defn find-best-store
  [cl,coll]
  ;trovo il magazzino a distanza minore da cl, ritorno costo e nodo magazzino in un array 
  (second (apply min-key #(first %) (map #(vector (computeCost cl %) %) coll))))

(defn find-best-stores
  [cl,coll]
  ;trovo i magazzini a distanza minore da cl, ritorno costo e nodo magazzino in un array 
  (into [] (map second (sort-by first (into [] (map #(vector (computeCost cl %) %)  coll))))))

;;get the all set in the subSetArray
(defn getAllSet
  [setArray]
  ;;all the store map in the subSetArray
  (map (fn [x] (get-in x [:set]) ) setArray))


(defn compCoverRatio
  [s1]
  (/ (count s1) (count clients))
)

(defn compSetCost
  [s1]
 
  (let [buildCost (get-in s1 [:store :build]) coverRatio (compCoverRatio s1)]
    (/ coverRatio (+ (MST-cost s1) buildCost)))
)



(defn calcDemand
  [set]
  (let [s (:set set)] (reduce + (map #(:capacity %) s))))

(defn mergeP
 [pi Jel]
 (assoc pi :set (set/union (getSet pi) (getSet Jel)))

)

(defn checkOcc
  [pi J]
  (def return false)
  (doseq [Jel J]
    (if (= (:store pi) (:store Jel))
      (def return (mergeP pi Jel))  
      ))
  return
)



(defn rIdx
 [pi J]
 (.indexOf (:store pi) (into [] (map #(select-keys % [:store]) J)))
 )

(defn remove-duplicates
  [Jin]
  (def J Jin)
  (doseq [cl clients]
    (let [presence (into [] (remove #(nil? %) (map (fn [x] (if (contains? (:set x) cl) x)) J)))]
      (if (> (count presence) 1)
        (do 
          (def tmp (into [] (remove #(= (:store %) (find-best-store cl (into [] (map :store presence)))) presence)))
          (doseq [p tmp]
            (def J (assoc-in J [(.indexOf J p) :set] (set/difference (:set p) #{cl}))))
          ))))
  J)

(defn evaluate
  [in]
   (+ (reduce + (map #(tour-cost (:tour %)) in)) 
      (reduce + (map #(get-in % [:store :build]) in)))
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

  (def optimum)
  (def optimumCost Double/MAX_VALUE)
  (def improved false)
  (def ngrasp 0)
  (def nswap 0)
  (def nstore 0)

  (instance-init)
  (println "Massima capacità: "storeCapacity " Massima domanda: "maxDemand)
  (println "Numero magazzini tot: "numPossMag)

  (loop [idx0 1]
   
    (if (= nswap 1)
      (do
        (println "\t\t*****STORE SWAP******")
        (def cover (swap-store optimum))))


    (if (and (= ngrasp 10) (< nswap 1))
      (do
        (println "SWAP ")
        (def cover (k-swap optimum))
        (println (map calcDemand cover))))

    (if (< ngrasp 10)
      (do
        (println "CONSTR GREEDY SOL ")
        (def cover (constrGreedySol))))

    (println "Nstore " nstore "Nswap " nswap "Ngrasp " ngrasp)
    (def candidate (local-search cover))
    (def candidateCost (evaluate candidate))
    (print candidateCost " ?< " optimumCost)
    (if (< candidateCost optimumCost)
      (do
        (def improved true)
        (def optimum candidate)
        (def optimumCost candidateCost))
      (def improved false))
    (println  " : " improved)

    (if (= nswap 1)
      (if improved 
        (do
          (def nstore 0)
          (def nswap 0))
        (def nstore (inc nstore))))

    (if (and (= ngrasp 10) (< nswap 1))
      (if improved 
        (def nswap 0)
        (def nswap (inc nswap))))
    
      (if (< ngrasp 10)
        (if improved 
          (def ngrasp 0)
          (def ngrasp (inc ngrasp))))


      (println "\n\n")

    (if (< nstore 1)
      (recur (inc nstore))))

)

