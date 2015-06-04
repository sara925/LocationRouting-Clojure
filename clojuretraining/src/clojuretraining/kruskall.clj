(in-ns 'clojuretraining.core)
;;(use 'jordanlewis.data.union-find)

;;funzione per la creazione della 
(defn setToUn
  "Function to convert a Set of Maps to a Union-find 
  to perform the Kruskall Algorithm"
  ;;dal set pkesima all'union find
  [pk]
  (def un (union-find))
  ;;scorro tutto il set e riaggiorno un
  ;;come la conj dell'union-find e la mappa del cliente
  (doseq [cli pk]
    (def un (conj un cli)))
  un
)

(defn linkCosts 
  [m1 m2]
  (def ret)
  (let [cost (computeCost m1 m2)]
    (def ret [[(:x m1) (:y m1)] [(:x m2) (:y m2)] cost])
    )
  ;;(println ret)
  ret
)

(defn prova 
[x set]
(map #(linkCosts x %) (subvec set x))
)

(defn linkArray
  [set]
  (def linkarray [])
  ;;il set deve contenere anche il magazzino
  (def ret)
  (doseq [x set]
    (def ret (conj ret (fn [x]
                         (def ret1 (map #(linkCosts x %) (subvec set x)))
                         ret1 ))
)
    )
  ret
)


