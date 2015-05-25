(ns clojure-training.core
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:gen-class))


;;---general purpose data structures--
(def nodeMaps [])   ;array of maps, each map represents a node read from a TSP benchmark
(def numPossMag)    ;number of possible storehouse locations 
(def storeCapacity) ;number representing the maximum storehouse capacity, it is the same for every storehouse
(def maxDemand)     ;every customer's demand varies from 1 to maxDemand (default: [1,20])  
(def stores [])     ;an array of numPossMag nodes randomly choosen in nodeMaps
(def clients [])    ;array of customers locations obtained as nodeMaps - stores
;;---greedy function and GRASP data structures--
(def subSetArray []);contains a random number of randomly choosen subsets of clients, every subsets represents the binding between a storehouse and its customers





;;-----------PARSING FUNCTIONS-----------------
(defn initNode 
  "nodeMaps initialization: nodeMaps is an array of maps, every map represents a node of the TSP"
  [seq]
  (def nodes (zipmap [:id :x :y :capacity] [(first seq) (second seq) (last seq) 0]))
  (def nodeMaps (conj nodeMaps nodes))
  )

(defn parse
"Read the TSP benchmark file and initialize the nodeMaps array"
  [lines]
  (doseq [line lines] ;;iterates over all lines in file
    (let [lineSplit (take 3 (str/split  line #"\s"))] 
            (if (number? (read-string (first lineSplit)))
              (initNode lineSplit) 
            )        
))) 


(defn parseParam
  "Read and parse the problem's parameters from an input file (given by command line)"
  [seq]
  (doseq [line seq]
    (let [lineSplit (take 2 (str/split line #":"))]
      (if (= (str/trim(first lineSplit)) (str/trim "STORE CAPACITY"))
        (def storeCapacity (read-string (second lineSplit))))
      (if (= (str/trim(first lineSplit)) (str/trim "MAXIMUM DEMAND"))
        (def maxDemand (read-string (second lineSplit))))
    )
))
 

;;-----------INSTANCE INITIALIZATION FUNCTIONS--
(defn unique-rand-int-set
  "Return a set of unique numElem random numbers in the range [0-maxVal] "
  [maxVal,numElem]
  (let [a-set (set (take numElem (repeatedly #(rand-int maxVal))))]
    (concat a-set (set/difference (set (take numElem (range))) a-set))
))

(defn instanceInit
  "Instance initialization: 
   computation of numPossMag, stores and clients arrays initialization"
  []

  ;;First a lower bound to the number of storehouses needed to exaust the customers' demand is computed as
  ;;   lower_bound = (numberOfCustomers * maxDemand)/storeCapacity
  ;;Then the number of possible storehouse location is randomly choosen in the range [lower_bound, lower_bound+x] 
  ;; where x is a random number between 0 and 20
  (def numPossMag (+ (rand-int 20)   
                     (quot (* (count nodeMaps) maxDemand) storeCapacity))) 
  
  ;;select numPossMag indexes from the nodeMaps array 
  (def randIdx (unique-rand-int-set (count nodeMaps) numPossMag))
  (loop [iter 1]
    ;;rand-int return a number between 0 and nodeMaps
    ;;in the loop rand-int return the same number more than 1 time
    ;;the execution let see store with capacity > 600
    ;;to-fix we have to consider the n already selected in the
    ;;previous loop iteration    ---> da cambiare? o tenere come memo?
    (let [n (nth randIdx iter)]
      (def nodeMaps (update-in nodeMaps [n :capacity] + storeCapacity));;update the capacity of every storehouse location to storeCapacity
      (def stores (conj stores (get nodeMaps n)))
    )
    (if (< iter numPossMag)
      (recur (inc iter))
  ))

  ;;clients array initialization: set(nodeMaps) - set(stores)
  (def clients (into [] (set/difference (set nodeMaps) (set stores))))
  ;;capacity update
  (loop [iter 0]
    (let [num  (+ 1 (rand-int (- maxDemand 1)))] 
      (def clients (update-in clients [iter :capacity] + num)) ;;assign to every customer a demand between 1 and maxDemand
    ) 
    (if (< iter (- (count clients) 1))
      (recur (inc iter))
  ))
)

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
 (println x1 x2)
 (Math/pow (- (Float/parseFloat x1) (Float/parseFloat x2)) 2)
)

(defn computeCost 
  [map1 map2]
  (Math/sqrt (+ (ediff (:x map1) (:x map2)) (ediff (:y map1) (:y map2))))
)

(defn compSetCost
  [s1]
  (let [store (get stores (s1 :S))]
    (println  (some #(if (number? %)%) s1))
   ; (computeCost store (second s1))
  )
)

(defn constrGreedySol
 "Construction of a greedy solution to the problem, the greedy solution is used as input to the local search procedure.
  The solution is computed using a set covering approach"
  []
  
  (initSubSetArray)
  (def costs [])
  (doseq [pk subSetArray] 
    (compSetCost pk))
  
 ; (println (count (first subSetArray)))
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
    (with-open [rdr (io/reader (str "./resources/"(first args)))]
      (parseParam (line-seq rdr)))
    )

  ;;apro il file e lo leggo una riga alla volta, lo passo poi al parser
  ;; per l'inizializzazione della struttura dati che conterrà tutti i nodi
  (with-open [rdr (io/reader "./resources/lu980.txt")]
    (parse (line-seq rdr)))
  
  ;;instance initialization
  (instanceInit)
  
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
