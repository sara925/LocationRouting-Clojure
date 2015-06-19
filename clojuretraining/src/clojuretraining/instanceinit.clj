(in-ns 'clojuretraining.core)

 
;;leggi il file e inizializza nodeMaps
(defn read-benchmark-file 
 "Leggo un file di nodi e creo la struttura nodeMaps"
 [fileName]

 (with-open [rdr (io/reader (str "./resources/" fileName))]
   (let [lines (line-seq rdr)]

     (doseq [line lines] 
       (let [nodeVal (map read-string (take 3 (str/split  line #"\s")))] 
         (if (number? (first nodeVal))

            (def nodeMaps (conj nodeMaps (zipmap [:id :x :y :capacity] [(first nodeVal) (second nodeVal) (last nodeVal) 0]))))
       ))
   ))
 (def tmp #{})
 (println "Nodi nodeMaps prima della rimozione " (count nodeMaps))
 (doseq [a nodeMaps b nodeMaps] (if  (and (= (:x a) (:x b)) (= (:y a) (:y b)) (not= (:id a) (:id b))) (def tmp (set/union  tmp #{a b}))))
 ;;only non-duplicate node
 (def nodeMaps (into [] (set/difference (set nodeMaps) tmp)))
  (println "Nodi nodeMaps non duplicati " (count nodeMaps))
)


;;leggi parametri
(defn read-param-file
	"Legge un file di parametri e inizializza le apposite strutture"
	[fileName]

	(with-open [rdr (io/reader (str "./resources/" fileName))]
 		(let [lines (line-seq rdr)]

			(doseq [line lines]
				(let [lineSplit (take 2 (str/split line #":"))]
					;;parsing different cases
					(if (= (str/trim(first lineSplit)) (str/trim "STORE CAPACITY"))
					  (def storeCapacity (read-string (second lineSplit))))

					(if (= (str/trim(first lineSplit)) (str/trim "MAXIMUM DEMAND"))
					  (def maxDemand (read-string (second lineSplit))))
				))
		))
)



;;inizializza i magazzini
(defn unique-rand-int-set
  "Return a set of unique numElem random numbers in the range [0-maxVal] "
  [maxVal numElem]

  (let [a-set (set (take numElem (repeatedly #(rand-int maxVal))))]
    (concat a-set (set/difference (set (take numElem (range))) a-set)))
)


(defn create-store-locations
	"Seleziona da nodeMaps i nodi che saranno possibili location di magazzini"
	[]

  (def numPossMag (+ (rand-int 20)   
                     (quot (* (count nodeMaps) maxDemand) storeCapacity))) 

  (def randIdx (unique-rand-int-set (count nodeMaps) numPossMag))

  (loop [iter 1]
		(let [n (nth randIdx iter)]
      (def nodeMaps (update-in nodeMaps [n :capacity] + storeCapacity))
      (def stores (conj stores (get nodeMaps n))))

    (if (< iter numPossMag)
      (recur (inc iter))))
)



;;inizializza i clienti
(defn create-customers-array
	"Seleziona i nodi da nodeMaps che non sono storehouse"
	[]
	
	(def clients (into [] (set/difference (set nodeMaps) (set stores))))

  (loop [iter 0]
    (let [num  (+ 1 (rand-int (- maxDemand 1)))] 
      (def clients (update-in clients [iter :capacity] + num)))
 
    (if (< iter (- (count clients) 1))
      (recur (inc iter))))
)


;;inizializzazione istanza
(defn instance-init
	"Inizializza l'istanza"
	[]
	
	(create-store-locations )
	
	(create-customers-array)

	(def stores (vec (map #(assoc % :build (+ 10000 (rand 10000))) stores))) 
)
