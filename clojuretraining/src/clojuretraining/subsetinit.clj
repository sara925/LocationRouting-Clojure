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

