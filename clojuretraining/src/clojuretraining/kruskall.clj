(in-ns 'clojuretraining.core)
;;(use 'jordanlewis.data.union-find)

;;funzione per la creazione della 


(defn linkCosts 
  [m1 m2]
  (def ret)
  (let [cost (computeCost m1 m2)]
    (def ret [{:x (:x m1) :y (:y m1)} {:x (:x m2) :y (:y m2)} cost])
    )
  ret
)



(defn linkArray
  [set]
  (def linkArray [])
  ;;il set deve contenere anche il magazzino
  (doseq [x set]
    (doseq [y (subvec set (+  1 (.indexOf set x)))]
      (def linkArray (conj linkArray (linkCosts x y))))
  )
  linkArray
)

(defn make-union-find [nodes]
  ;;TODO da cambiare non restiusce una notazione di mappa
  (apply hash-map (mapcat (fn[x][x [x [x]]]) nodes)))

(defn joined? [union-find a b]
  (println "uf a" (union-find a) "uf b"  (union-find b)  )
  ;;(println "union-find" union-find)
  ;;(def mmm (union-find a))
  ;;(println "mmm a" mmm)
  ;;(println "f a" (union-find  a) "f b"(union-find  b)  )
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

(defn MST
  [set]
  
  ;calcolo gli archi per costo crescente (costo= eucl.dist.)
  (def links (linkArray set))
  (def links (sort-by (fn[[_ _ cost]] cost) links))
  (def tree  (map (fn [x] (select-keys x [:x :y]) ) set))
  (def tree  (make-union-find tree))
  
  ;MST
  (reduce + (map (fn [[_ _ x]] x) (second (reduce add-link [tree '()] links))))
  ;(second (reduce add-link [tree '()] links))
)


