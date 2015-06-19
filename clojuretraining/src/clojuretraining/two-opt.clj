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
 (def s node)
 (def frontiera #{})
 (def esplorati #{})
 (def rimfront #{})
 (loop [iter 0]
   (def vicini (find-neighboors s graph))
   (def esplorati (set/union (set vicini) esplorati))
   (def frontiera (set/union frontiera 
                             (into #{} (map (fn [[a b _]] (if (= s a) b a)) vicini))))
   (def frontiera (set/difference frontiera #{s}))
   (def rimfront (set/union rimfront #{s}))
   (def frontiera (set/difference frontiera rimfront))
   (def s (first frontiera))
   (if (not (empty? frontiera))
    (recur (inc iter))))
(count esplorati))

(defn dfs
  [graph node]
  (def nn node)
  (def gr graph)
   (loop [vertices [] explored #{nn} frontier [nn]]
    (if (empty? frontier)
      vertices
      (let [v (first frontier)
            neigb (gr v)]
        (recur
         (conj vertices v)
         (into explored neigb)
         (into (pop frontier) (remove explored neigb)))
        ))
    )
)


(defn creaGrafo
  [gr,node]
  (def nn node)
  (def nodi (distinct (mapcat (fn [[a b _]] [a b]) gr)))
  (def G {})
  (doseq [n nodi]
    (def G (conj G [n (into [] (remove nil? (map (fn [[a b _]] 
                                                   (if (= a n) b a)) 
                                                 (find-neighboors n gr))))])))
  
  (count (distinct (dfs G node)))
)





(defn not-bridge?
  [start,graph,r]
  (def ret )
  (if (> (creaGrafo graph start)
         (creaGrafo (into '() (remove #(= r %) graph)) start) )
    (def ret false)
    (def ret  true))
(println   (creaGrafo graph start)   (creaGrafo (into '() (remove #(= r %) graph)) start) ret)
ret
)


(defn fleury
  [gr]
  (def p gr)
  (def f '())
  (def toRem [])
  (def start (first (first p))) ;arbitrary starting point
  (loop [idx 0]
    (println start)
    (def neigh (find-neighboors start p))
    (if (= (count neigh) 1)
      (do
        (def toRem (first neigh))
        (def p (into '() (remove #(= % toRem) p )))
        (def f (conj f toRem))))
    (if (> (count neigh) 1) 
      (do
        (println "count >1 " (count neigh))
        (def toRem (some #(when (not-bridge? start p %) %)
                         (find-neighboors start p)))
        
        (def f (conj f toRem))
        (def p (into '() (remove #(= % toRem) p)))
        ))
 
    ;(println toRem)
    (if (= start (first toRem))
      (def start (second toRem))
      (def start (first toRem)))
   (println (count p))
   (if (and (< idx 200) (not (empty? p)))
     (recur (inc idx))))
  
  ;f
  '()
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
