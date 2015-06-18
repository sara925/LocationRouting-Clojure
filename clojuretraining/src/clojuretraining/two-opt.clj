(in-ns 'clojuretraining.core)



(defn tour-cost
  [tour]
  ;;TODO eventualmente chiamare tour-cost anche in MST-cost
  (reduce + (map (fn [[_ _ x]] x) tour))
)


(defn add-store-to-set
  [s]
  
  (doseq [idx (range (count s))]
    (def cover 
      (assoc-in cover [idx :set] 
                (set/union (:set s) #{(:store s)})))))


(defn find-best-matching
  [cl,coll]
  ;ritorno costo e nodo
  (apply min-key #(first %) (map #(vector (computeCost cl %) %) coll)))


(defn perfect-matching
  [[node & rest], ret]
  ;;gli archi sono nella forma [a b costo]
   (def ret1 ret)
   (def rest1 rest)
   (def c (find-best-matching node rest1))
   (def ret1 (conj ret1 [node (second c) (first c)]))
   (def rest1 (into '() (remove #(= % (second c))rest1)))
   (if (not (empty? rest1))
     (perfect-matching rest1 ret1))
   ret1
)

(defn find-neighboors
  [node graph]
  (into '() (remove nil? (map (fn [x] (if (some #(= node %) x) x)) graph)))
)


(defn dfs-count
 [node graph]
 (def start node)
 (def frontiera #{})
 (def esplorati #{})
 (def rimfront #{})
 (loop [iter 0]
   (def vicini (find-neighboors start graph))
   (def esplorati (set/union (set vicini) esplorati))
   (def frontiera (set/union frontiera 
                             (into #{} (map (fn [[a b _]] (if (= start a) b a)) vicini))))
   (def frontiera (set/difference frontiera #{start}))
   (def rimfront (set/union rimfront #{start}))
   (def frontiera (set/difference frontiera rimfront))
   (def start (first frontiera))
   (if (not (empty? frontiera))
    (recur (inc iter))))
(count esplorati))



(defn not-bridge?
  [start,graph,r]
  ;;if 
  (def ss start)
  (if (> (dfs-count ss graph)
         (dfs-count ss (into '() (remove #(= r %) graph))))
    false
    true)

)


(defn fleury
  [gr]
  (def p gr)
  (def f '())
  (def toRem)
  (def start (first (first p)))
  (loop [idx 0]
    (println start)
    (def neigh (find-neighboors start p))
    (if (= (count neigh) 1)
      (do
        (println "count 1")
        (def toRem (first neigh))
        (def p (into '() (remove #(= % toRem) p )))
        (def f (conj f (first neigh))))
      (do
        (print "count > 1")
        (def toRem (some #(when (not-bridge? start p %) %) (find-neighboors start p)))
        (println "toRem" toRem)
        (println (count p))
        (def f (conj f toRem))
        (println start)
        (def p (into '() (remove #(= % toRem) p)))))
        (def start (first (first p)))
        ;(println (first toRem) (second toRem))
    (if (not (empty? p))
      (recur (inc idx))))
  ;;calcolato ciclo euleriano
  f
)

 (defn take-shortcut
   [eulin]
   (def eul eulin)
   ;;nodi del cammino che hanno r >2
   (map (fn [x] find-neighboors x eul) (mapcat (fn [[a b _]] [a b]) eul) )
   )

(defn christofides
  [s]
  
  (def mst (MST s))
  (def odd (map first (filter (fn [[_ y]] (odd? y)) 
                       (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} 
                               (mapcat (fn [[a b _]] [a b]) mst))))) ;nodi del set di grado dispari nell'MST
 
  (def odd (perfect-matching odd '())); archi del perfect-matching su odd
  (doseq [a odd]
    (def mst (conj mst a)))

  ;;(def mst (conj (into [] mst) (into [] odd))) ;aggiungo all'mst gli archi del matching, 
  ;(def mst (into '() mst))
  ;(println "vai fleury")
  (def eul (fleury mst))
  '()
  ;(take-shortcut eul)
)

(defn prova
  [J]
  
  (def cover J)
  (def cover (add-store-to-set cover))

  ;mi serve un ciclo da cui partire
  
  ;;da qui in poi lavoro sul set singolo
  (def setProva (first cover))
  
  (christofides setProva)

)
