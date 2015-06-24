(in-ns 'clojuretraining.core)

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

(defn getAllStore
  [setArray]
  ;;all the store map in the subSetArray
  (map (fn [x] (get-in x [:store]) ) setArray))

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
  (def notAssigned (set/difference (set clients) 
                                   (set/intersection (set clients) (reduce set/union (getAllSet subSetArray)))))
  (println "Clienti non assegnati " (count notAssigned))
  (if (not (empty? notAssigned))
    (doseq [cl notAssigned] (assign-to-set cl))  
  )
)


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

  ;"rifinisco" J
  (def J (into [] (remove #(empty? (:set %))  J)))
  (def J (remove-duplicates J))
  (def J (into [] (remove #(empty? (:set %))  J)))
 
  (loop [] 
    (def J (fixCapacity J))
    (if (some #(> % storeCapacity) (map calcDemand J))
      (recur)))

  (println (map calcDemand J))

  J)
