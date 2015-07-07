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
;;store cost parameter
(def buildCost)
(def buildRange)



(def borders {:xmin 0 :xmax 0 :ymin 0 :ymax 0})
(def slotsProbability (reductions + [50 30 15 5]))  ;probabilità delle varie fasce
;;----------------------------------------------

(defn getAllStore
  [setArray]
  ;;all the store map in the subSetArray
  (map (fn [x] (get-in x [:store]) ) setArray))

(load "two-opt")

(defn createSubSet 
 [slots, subSet]
 
 (def tmp subSet) 
 (def freeSpace storeCapacity)
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
  (map (fn [x] (get-in x [:set]) ) setArray)

)

(defn assign-to-set
  [cl]
  (def prescelto (find-best-store cl stores))
  (def pSet ((group-by :store subSetArray) prescelto))
  (def pSet (get pSet (rand-int (count pSet))))
  (def idx (.indexOf subSetArray pSet))

   (let [foglie (MST-leaf pSet)]
    (loop [iter 0]
      (def foglia (nth foglie (rand-int (count foglie))))    
      (def found  (contains? (reduce set/union (set (getAllSet (remove #(= pSet %) subSetArray)))) foglia))
      (if found
        (do
          (def subSetArray (assoc-in subSetArray [idx :set] (into #{cl} (remove #(= foglia %) (getSet pSet))))) ;sostituisce cl a foglia
        ))

      (if (and (< iter (- (count foglie) 1)) (not found))
        (recur (inc iter))))
  )
)

(defn compCoverRatio
  [s1]
  (/ (count s1) (count clients))
)

(defn compSetCost
  [s1]
 
  (let [buildCost (get-in s1 [:store :build]) coverRatio (compCoverRatio s1)]
    (/ coverRatio (+ (MST-cost s1) buildCost)))
)


(defn initSubSetArray
 []
  (def subSetArray [])
  (find-border-customers)
  (doseq [iter (range (count stores))] 
    
    (def slots (assignProbability (get stores iter)))
    (doseq [x (range 3)]
      ;;costruzione del subset come insieme di mappe clienti e mappa store
      ;;(def subSet #{ {:store (get stores iter)} {:x 1 :y 2} }) ;TODO aggiungi il set
      (def subSet #{})
      (def subSet (createSubSet slots subSet))
      ;;modifica del subset nella struttura
      ;{: store nodo_magazzino :set insieme dei nodi del set compreso nodo maggazino}
      (def subSet {:store (get stores iter) :set subSet})
      (def subSetArray (conj subSetArray subSet)))    
  )
  (def subSetArray (into [] (map #(assoc  %1 :id %2)  subSetArray (range (count subSetArray)))))
  ;;la struttura è quella voluta
  ;;definisco una function per utilizzare solo la mappa set(clienti+store nelle varie funzioni)
  (def notAssigned (set/difference (set clients) (set/intersection  (set clients) (reduce set/union  (getAllSet subSetArray) ))))
  (print "  Clienti non assegnati ad alcun set " (count notAssigned))
  (if (not (empty? notAssigned))
    (do 
      (doseq [cl notAssigned] (assign-to-set cl))
      (print " -> 0"))  
  )
  (println "")
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


(defn fixCapacity
  [Jin]
  (def J Jin)
  (doseq [i (range (count J))]
    (if (> (calcDemand (get J i)) storeCapacity)
      (do
        (def foglie (MST-leaf (get J i)) )
        (loop [toRemove (- (calcDemand (get J i)) storeCapacity)]
          (def foglia (nth foglie (rand-int (count foglie))))
          
          (def s 
            (find-best-stores 
               foglia 
               (getAllStore (into [] (remove nil? (into [] 
                                   (map (fn [x] (if (< (+ (calcDemand x) (:capacity foglia)) storeCapacity) x)) J )))))))
    
          (def idx (.indexOf J (first (filter #(= (:store %) (first s)) J))))
          ;;rimozione della foglia
          (def J (assoc-in J [i :set] (set/difference (:set (get J i)) #{foglia})))
          ;;aggiunta in quello 
          (def J (assoc-in J [idx :set] (set/union (:set (get J idx)) #{foglia})))
          (if (> toRemove 0)
            (recur (- toRemove (:capacity foglia)))))
      )))
  J)

(defn constrGreedySol
  []
  (println "Costruzione dei un cover ammissibile")
  (initSubSetArray)
  (def subSetTmp subSetArray)

  (def J [])
  (loop [iter 0]
    ;;calcolo pesi
    (doseq [idx (range (count subSetTmp))]
      (def subSetTmp (assoc-in subSetTmp [idx :cost] (compSetCost (get subSetTmp idx)))))

    (def subSetTmp (sort-by :cost subSetTmp))
    (def beta (* 1.3 (:cost (first subSetTmp))))
    (let [P (filter #(< (:cost %) beta) subSetTmp)
          pi (nth P (rand-int (count P)))
          ret (checkOcc pi J)]
      (if (false? ret)
        (def J (conj J (get subSetArray (.indexOf subSetArray (first (filter #(= (:id pi) (:id %)) subSetArray))))))
        (def J (assoc J (.indexOf J (first (filter #(= (:store pi) (:store %)) J))) ret)))
      
      (def subSetTmp (into [] (map (fn [x] (assoc x :set (set/difference (:set x) (:set pi)))) subSetTmp)))
      (def subSetTmp (into [] (remove #(empty? (:set %)) subSetTmp)))
      
      ;(println "Rimasti "(count (reduce set/union (getAllSet subSetTmp))))
      ) 
    
    (if (not (empty? (reduce set/union (getAllSet subSetTmp))))
      (recur (inc iter)))
    )
  ()
  ;"rifinisco" J
  (def J (into [] (remove #(empty? (:set %))  J)))
  (println "  Controllo e rimozione clienti duplicati..")
  (def J (remove-duplicates J))
  (def J (into [] (remove #(empty? (:set %))  J)))
 
  (println "  Controllo e ripristino vincolo capacità del magazzino..")
  (loop [] 
    (def J (fixCapacity J))
    (if (some #(> % storeCapacity) (map calcDemand J))
      (recur)))

  ;(println (map calcDemand J))
  (println "  Numero di magazzini del cover " (count J) "/"numPossMag)
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
        (def maxDemand 20)
        (def maxgrasp 10)
        (def maxIls 10)
        (def maxnstore 1)
        (def buildCost 10000)
        (def buildRange 10000)) 
    ;;read values from input file
	(read-param-file (str "./resources/"(first args))))

  ;;apro il file e lo leggo una riga alla volta, lo passo poi al parser
  ;; per l'inizializzazione della struttura dati che conterrà tutti i nodi
  (read-benchmark-file "lu980.txt") 

  (def optimum)
  (def optimumCost Double/MAX_VALUE)
  (def improved false)
  (def ngrasp 0)
  (def nstore 0)
  ;;maxswap maxgrasp maxnstore are the maximum iterations
  ;;for each fallimentar procedure
  (println "Lettura del file di benchmark e inizializzazione istanza...")
  (instance-init)
  (println "Capacità magazzini: "storeCapacity "\nMassima domanda per cliente: "maxDemand)
  (println "Numero magazzini tot: "numPossMag "\n\n")

  (loop [idx0 1]
   
    (if (= ngrasp maxgrasp)
      (do
        (println "\t**************** STORE SWAP **************** ")
        (def cover (swap-store optimum))
        (def maxIls 20)
        (println (map calcDemand cover))  
        ))

    (if (< ngrasp maxgrasp)
      (do
        (println "\t****************   GRASP   ****************")
        (def cover (constrGreedySol))))

    (println "\nStore-Swap failures "nstore"/"maxnstore "GRASP failures "ngrasp"/"maxgrasp)
    (def candidate (local-search cover))
    (def candidateCost (evaluate candidate))
   
    (println "    COSTO X'=LS(X)         COSTO X* ")
    (if (< optimumCost Double/MAX_VALUE) 
      (print "   "(format "%8.4f" candidateCost) "   ?<   " (format "%8.4f" optimumCost))
      (print "   "(format "%8.4f" candidateCost) "   ?<   " optimumCost))
    (if (< candidateCost optimumCost)
      (do
        (def improved true)
        (def optimum candidate)
        (def optimumCost candidateCost))
       (def improved false)
    )
    (println " "improved)
    (if (or improved (< (- candidateCost optimumCost) buildCost))  
      (do
        (println "\t\tStarting ILS...")
        (loop [idx1 1]
          ;;k-swap ammissibile
          (loop []
            (def destr (k-swap candidate))
            (if (some #(> % storeCapacity) (map calcDemand destr))
              (recur)))

          (def destr (local-search destr))
          (def destrCost (evaluate destr))
          (print " "idx1":"(format "%8.4f" destrCost) "   ?<   " (format "%8.4f" candidateCost))
         ; (println idx1 ": " destrCost "<? " candidateCost)
          (if (< destrCost candidateCost) 
            (do 
              (println "  true")
              (def candidate destr)
              (def candidateCost destrCost))
              (println "  false"))
          (if (< candidateCost optimumCost)
            (do 
              (def optimum candidate)
              (def optimumCost candidateCost)
              (def improved true)))

          (if  (< idx1 maxIls )
            (recur (inc idx1))))))
   
    (println  "Ottimo candidato costo: " optimumCost)

    (if (= ngrasp maxgrasp)
      (if improved 
        (def nstore 0)
        (def nstore (inc nstore))))
  
      (if (< ngrasp maxgrasp)
        (if improved 
          (def ngrasp 0)
          (def ngrasp (inc ngrasp))))


      (println "\n\n")

    (if (< nstore maxnstore)
      (recur (inc idx0))))

)

