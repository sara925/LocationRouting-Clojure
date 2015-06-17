(in-ns 'clojuretraining.core)
;;(use 'jordanlewis.data.union-find)

(defn linkCosts 
  [m1 m2]
  (def ret)
  (let [cost (computeCost m1 m2)]
    (def ret [{:x (:x m1) :y (:y m1) :capacity (:capacity m1) :id (:id m1)} 
              {:x (:x m2) :y (:y m2) :capacity (:capacity m2) :id (:id m2)} cost])
    )
  ret)


(defn linkArray
  [set]
  (def linka [])
  ;;il set deve contenere anche il magazzino
  (doseq [x set]
    (doseq [y (subvec set (+  1 (.indexOf set x)))]
      (def linka (conj linka (linkCosts x y))))
  )
  linka)

(defn make-union-find [nodes]
  (apply hash-map (mapcat (fn[x][x [x [x]]]) nodes)))

(defn joined? [union-find a b]
  (= (first (union-find a)) (first (union-find b))))


(defn join [union-find a b]
  (let [[leader-a _] (union-find a)
        [leader-b _] (union-find b)]
    (if (= leader-a leader-b) 
      union-find ;; nothing to do
      (let [[_ followers-a] (union-find leader-a)
            [_ followers-b] (union-find leader-b)]
        (if (>= (count followers-a) (count followers-b)) 
          (let [new-followers (into followers-a followers-b)] 
            (let [uf (assoc union-find leader-a [leader-a new-followers])] 
              (reduce (fn[uf x] (assoc uf x [leader-a []])) uf followers-b))) 
          (join union-find b a))))))


(defn add-link [[uf tree] link]
  (let [a (first link)
        b (second link)]
    (if (joined? uf a b) 
      [uf tree]
      [(join uf a b) (cons link tree)])))

;;get the set in the compisite map array
(defn getSet
  [cset]
  ;;return only the set map in the cset composite map
  (get-in cset [:set] ) 
)

(defn cSetToArray
  [set]
  ;;obtain the array of the set of clients and store nodes in
  ;;the composite set set
  (into [] (getSet set))
)

(defn MST-leaf
  [set]
  ;;the set parameter is in the composite form
  ;;use cSetToArrat function in the definition of links and tree
  ;;valutare se definire una nuova variabile o passsargli direttamente
  ;;la struttura rifinita
  ;;cSetToArray mi fornisce un Array utilizzabile da MST
  ;calcolo gli archi per costo crescente (costo= eucl.dist.)
  (def links (linkArray (cSetToArray set)))
  (def links (sort-by (fn[[_ _ cost]] cost) links))
  (def tree  (map (fn [x] (select-keys x [:x :y :capacity :id]) ) (cSetToArray set)))
  (def tree  (make-union-find tree))
  
  (def foglie  (map first (filter (fn [[_ y]] (= 1 y)) 
                                  (reduce #(assoc %1 %2 (inc (%1 %2 0))) {} (mapcat (fn [[a b _]] [a b]) 
                                                                                    (second (reduce add-link [tree '()] links)))))))
  (remove #(= (:id %) (:id (get-in set [:store]))) foglie)
)

(defn MST-cost
  [set]
  ;;the set parameter is in the composite form
  ;;use cSetToArrat function in the definition of links and tree
  ;;valutare se definire una nuova variabile o passsargli direttamente
  ;;la struttura rifinita
  ;;cSetToArray mi fornisce un Array utilizzabile da MST
  ;calcolo gli archi per costo crescente (costo= eucl.dist.)
  (def links (linkArray (cSetToArray set)))
  (def links (sort-by (fn[[_ _ cost]] cost) links))
  (def tree  (map (fn [x] (select-keys x [:x :y :capacity :id]) ) (cSetToArray set)))
  (def tree  (make-union-find tree))
  
  ;MST: ritorno il costo dell'MST
  (reduce + (map (fn [[_ _ x]] x) (second (reduce add-link [tree '()] links))))
)

(defn MST
  [set]
  ;;the set parameter is in the composite form
  ;;use cSetToArrat function in the definition of links and tree
  ;;valutare se definire una nuova variabile o passsargli direttamente
  ;;la struttura rifinita
  ;;cSetToArray mi fornisce un Array utilizzabile da MST
  ;calcolo gli archi per costo crescente (costo= eucl.dist.)
  (def links (linkArray set))
  (def links (sort-by (fn[[_ _ cost]] cost) links))
  (def tree  (map (fn [x] (select-keys x [:x :y :capacity :id]) ) (cSetToArray set)))
  (def tree  (make-union-find tree))
  
  ;MST: ritorno l'MST come lista di [nodoA nodoB costo]
   (second (reduce add-link [tree '()] links))
)
