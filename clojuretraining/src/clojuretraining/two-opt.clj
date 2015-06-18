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

(defn perfect-matching
  [odd]
  ;;gli archi sono nella forma [a b costo]
  ;;TODO
  '()
)

(defn find-neighboors
  [node graph]

  (into '() (remove nil? (map (fn [x] (if (some #(= node %) x) x)) graph)))
)


(defn dfs-count
 [node graph]
 
 

)

(defn not-bridge?
  [graph u edge]
  
  (dfs-count u graph)

  (dfs-count u (remove #(= edge %) graph))

)

(defn recur-adjacent
  [u, graph]

  (def pass graph)
  (def neighboors (find-neighboors u graph))                   ;vicini di u
 
  (if (= 1 (count neighboors)) 
     ;se ha un solo vicino brucio il ponte..
    (def pass (remove #(= (first neighboors) %) pass ))
     ;se ha più di un vicino
    (do 
      (doseq [neigh neighboors]
        (if (not-bridge? pass u neigh)
          (def pass (remove #(= neigh %) pass))))
      ))
 
  pass
)

(defn fleury
  [gr,[[start _ _] & rest]]
  
  (def p gr)
  (def f gr)
  (loop [idx 0]

    (recur-adjacent start p)


    ;;posso attraversare (u,v) ?
    ;;se si rimuovilo da past e marchialo in future
    ;;aggiorna start

    (if (not (empty? p))
      (recur (inc idx))))

)

(defn christofides
  [s]
  
  (def mst (MST s))
  (def odd (map first (filter (fn [[_ y]] (odd? y)) 
                       (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} 
                               (mapcat (fn [[a b _]] [a b]) mst))))) ;nodi del set di grado dispari nell'MST
 
  (def odd (perfect-matching odd)); archi del perfect-matching su odd
  (def mst (flatten (conj mst odd))) ;aggiungo all'mst gli archi del matching, 
  
  ;(fleury mst)
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
