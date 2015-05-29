(ns clojuretraining.core
	(:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])

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
;;----------------GREEDY FUNCTION------------
(defn initSubSetArray
 "...TODO..."
 []

 (def n (int (* numPossMag (+ 1(rand 0.5)))));;a random number of possible subsets
 (def freeSpace (vec (repeat n storeCapacity)))
 (def clients (shuffle clients))
 (loop [idx 0]
   (let [cli (get clients idx)]
     (def subSetArray (conj subSetArray (into #{} (vector cli))))
     (def freeSpace (assoc freeSpace idx (- (get freeSpace idx) (:capacity cli))))
     (def subSetArray (assoc subSetArray idx (conj (get subSetArray idx) (rand-int (- numPossMag 1)))))) 
  
   (if (< idx (- n 1))
     (recur (inc idx))))

 ;;ciclo sui clienti
 (loop [idx 0] 
     (let [repeat (+ 1 (rand-int 4))] 
     ;;ciclo repeat volte per individuare i subset
     (loop [k 0]
       (let [pk (rand-int n) cli (get clients idx) subSetPk (get subSetArray pk) freePk (get freeSpace pk)]
         (if (< k repeat) 
           (if (and (not(contains? subSetPk cli)) (>= freePk (:capacity cli))) ;;se ci sta..
               (do
                 (def subSetArray (assoc subSetArray  (int pk)             
                                         (set/union subSetPk (vector cli)))) ;;lo inserisco
                 (def freeSpace (assoc freeSpace pk (- freePk (:capacity cli))));; aggiorno capcità residua
                 (recur (inc k))) 
               (recur k))))
       ))

   (if (< idx (- (count clients) 1))
     (recur (inc idx)))
 )
)

(defn ediff
 [x1 x2]
 (Math/pow (- (Float/parseFloat x1) (Float/parseFloat x2)) 2)
)

(defn computeCost 
  [map1 map2]
  (Math/sqrt (+ (ediff (:x map1) (:x map2)) (ediff (:y map1) (:y map2))))
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

(defn constrGreedySol
 "Construction of a greedy solution to the problem, the greedy solution is used as input to the local search procedure.
  The solution is computed using a set covering approach"
  []
  ;;vector of cover J
  (def J [])
  (initSubSetArray)
  ;;local copy of subSetArray
  (def subSetL (vec subSetArray))
  (loop [iter 0]
    ;;loop body

    (def costs [])
    (doseq [idx (range (count subSetL))] 
      (def costs (conj costs (zipmap [:pos :cost] [idx (compSetCost (get subSetL idx))]))))
    (def costs (sort-by :cost costs)) ;;li ordino per costo crescente
    

    (def beta (* 1.3 (:cost (first costs))))
    (def  P (filter #(< (:cost %) beta ) costs))
    ;;index of P element to build cover J
    (def k (rand-int (- (count P) 1)))
    
    (let [pk (get subSetL k)]
     (def J (conj J pk))
     (doseq [i (range (count subSetL))] (def subSetL (assoc subSetL i (set/difference (nth subSetL i) pk))))
      )
    (println (first (first J)))
    ;;TO_DO mettere not
    (if (empty? (unionCollection subSetL)) 
      (recur (inc iter))))  
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
    (constrGreedySol)

    ;;local search proceduere
    ;;...TODO..

    (if (< iter 1)
      (recur (inc iter)))
    )

  (println "Massima capacità: "storeCapacity "\nMassima domanda: "maxDemand)
  (println "Numero magazzini tot: "numPossMag)
  
  ;; (println (set  stores))



)
