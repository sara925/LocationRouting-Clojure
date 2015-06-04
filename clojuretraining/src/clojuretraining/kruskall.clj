(in-ns 'clojuretraining.core)
;;(use 'jordanlewis.data.union-find)

;;funzione per la creazione della 


(defn linkCosts 
  [m1 m2]
  (def ret)
  (let [cost (computeCost m1 m2)]
    (def ret [[(:x m1) (:y m1)] [(:x m2) (:y m2)] cost])
    )
  ;;(println ret)
  ret
)



(defn linkArray
  [set]
  (def linkArray [])
  ;;il set deve contenere anche il magazzino
  (doseq [x set]
    (doseq [y (subvec set x)]
      (def linkArray (conj linkArray (linkCosts x y))))
  )
  linkArray
)


