(in-ns 'clojuretraining.core)



(defn tour-cost
  [tour]
  ;;TODO eventualmente chiamare tour-cost anche in MST-cost
  (reduce + (map (fn [[_ _ x]] x) tour))
)


(defn add-store-to-set
  [s]
  (def cover #{})
  (doseq [idx (range (count s))]
    (def cover 
      (assoc-in s [idx :set] 
                (set/union (:set (get s idx)) #{(:store (get s idx))}))))
  cover)


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

(defn bfs
  [g s]
  (def nn s)
  (def gr g)
  ((fn rec-bfs [explored frontier]
     (lazy-seq
      (if (empty? frontier)
        nil
        (let [v (first frontier)
              neighbors (gr v)]
          (cons v (rec-bfs
                   (into explored neighbors)
                   (into (pop frontier) (remove explored neighbors))))))))
   #{nn} (conj (clojure.lang.PersistentQueue/EMPTY) nn))


)

(defn creaGrafo-bfs
  [gr,node]
  (def nn node)
  (def nodi (distinct (mapcat (fn [[a b _]] [a b]) gr)))
  (def G {})
  (doseq [n nodi]
    (def G (conj G [n (into [] (remove nil? (map (fn [[a b _]] 
                                                   (if (= a n) b a)) 
                                                 (find-neighboors n gr))))])))
  (count (distinct (bfs G node)))
)

(defn creaGrafo-dfs
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

(defn remove-first
  [syb lst]
  (let [[before after]
          (loop [b [] a lst]
            (if (empty? lst) 
              [b a]
              (if (= syb (first a))
                [b (rest a)]
                (recur (cons (first a) b) (rest a)))))]
   (concat (reverse before) after)))

(defn not-bridge?
  [start,graph,r]
  (def ret )
  (def gb graph)
  (def sb start)
  (def rb r)
  (if (> (creaGrafo-bfs gb sb)
         (creaGrafo-bfs (remove-first rb gb) sb) )
    (def ret false)
    (def ret  true))
;(println   (creaGrafo-bfs gb sb)   (creaGrafo-bfs (remove-first rb gb) sb) ret)
;(println   (creaGrafo-dfs gb sb)   (creaGrafo-dfs (remove-first rb gb) sb) ret)

ret)


(defn fleury
  [gr]
  (def p gr)
  (def f '())
  (def toRem [])
  (def start (first (first p))) ;arbitrary starting point
  (loop [idx 0]
    (def neigh (find-neighboors start p))
    (if (= (count neigh) 1)
      (do
        (def toRem (first neigh))
        (def p (into '() (remove-first toRem  p )))
        (def f (conj f toRem))))
    (if (> (count neigh) 1) 
      (do
        (def toRem (some #(when (not-bridge? start p %) %)
                         (find-neighboors start p)))
        (def f (conj f toRem))
        (def p (into '() (remove-first toRem p)))
        ))
 
    (if (= start (first toRem))
      (def start (second toRem))
      (def start (first toRem)))

   (if  (not (empty? p))
     (recur (inc idx))))
  
  f)

 (defn make-hc
   [[node & rest] cycle]
   (def rt rest)
   (def c (conj cycle (linkCosts node (first rest))))
   (if (> (count rt) 1)
     (make-hc rt c)
     (do 
       (def se (into '() (map first (filter (fn [[_ y]] (= y 1)) 
                                              (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} 
                                                      (mapcat (fn [[a b _]] [a b]) c))))))
         (println "se" se)
         (conj c (linkCosts (first se) (second se))))))
     


(defn take-shortcut
  [eulin]
  (def eul eulin)
  ;;shortcut procedure
  (def visisted #{})
  ;;node list
  (def nl (into '() (distinct (mapcat (fn [[a b _]] [a b] ) eul))))
  (def hc (make-hc nl '()))
  hc)

(defn christofides
  [s]
  
  (def mst (MST s))
  (def odd (map first (filter (fn [[_ y]] (odd? y)) 
                       (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} 
                               (mapcat (fn [[a b _]] [a b]) mst))))) ;nodi del set di grado dispari nell'MST
  (def odd (perfect-matching odd '())); archi del perfect-matching su odd
  (doseq [a odd]
    (def mst (conj mst a)))

  (def eul (fleury mst))
  (take-shortcut eul)
)

(defn prova
  [J]
  
  (def cover J)
  (def cover (add-store-to-set cover))

  ;mi serve un ciclo da cui partire
  
  ;;da qui in poi lavoro sul set singolo
  (def setProva (first cover))
  
  (def hamCycle (christofides setProva))
)
