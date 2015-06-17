(in-ns 'clojuretraining.core)



(defn tour-cost
  [tour]
  ;;TODO eventualmente chiamare tour-cost anche in MST-cost
  (reduce + (map (fn [[_ _ x]] x) tour))
)


(defn add-store-to-set
  [cover]
  
  (doseq [idx (range (count cover))]
    (def cover 
      (assoc-in cover [idx :set] 
                (set/union (:set s) #{(:store s)})))))

(defn perfect-matching
  [odd]
  ;;gli archi sono nella forma [a b costo]
  ;;TODO
)

(defn christofides
  [s]
  
  (def mst (MST s))
  (def odd (map first (filter (fn [[_ y]] (odd? y)) 
                       (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} 
                               (mapcat (fn [[a b _]] [a b]) mst))))) ;nodi del set di grado dispari nell'MST
 
  (def odd (perfect-matching odd)); archi del perfect-matching su odd
  (def mst (conj mst odd)) ;aggiungo all'mst gli archi del matching

)

(defn prova
  [J]
  
  (def cover J)
  (def cover (add-store-to-set cover))

  ;mi serve un ciclo da cui partire
  
  ;;da qui in poi lavoro sul set singolo
  (def setProva (:set (first cover)))
  
  (christofides setProva)

)
