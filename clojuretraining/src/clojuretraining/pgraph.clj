(in-ns 'clojuretraining.core)

;function to print an hamiltonian cycle in a .dot file
;then the file is printable using graphviz

(defn printGraph
[hc file]
;;prepare the .dot format
(def initstr (str "graph G { \n ") )
;;obtain the list of node
(def nlist (distinct (into '() (mapcat (fn [[a b _]] [a b]) hc))))
;;each couple of node is an edge
(with-open [wrtr (io/writer (str "./resources/" file))]
 		(.write wrtr initstr )
                ;;start to print the eddge
                (doseq [edge hc]
                  (let [e (take 2 edge)]
                    (.write wrtr (str (:id (first e)) " -- " (:id (second edge)) " ;"  "\n") )               
                   )
            )
             ;print the coordinates on the graph
             ;scale 1:10 to print a viewable graph
            (doseq [n nlist]
              (.write wrtr (str (:id n)
                                " [\n label = " (:id n) "\n  pos=\"" (/  (:x n) 5) "," (/ (:y n) 5) "!\"\n]\n" ))
              )
   (.write wrtr "}")
)
;;end
)

(defn printSubSetFile
[hc, fileName]
;;definizione di stringa di inizializzazione
(def nlist (distinct (into '() (mapcat (fn [[a b _]] [a b]) hc))))
(def initstr (str "NAME : " fileName "\nCOMMENT : subSetCreated
TYPE : TSP
DIMENSION :" (count nlist) "\n" "EDGE_WEIGHT_TYPE : EUC_2D
NODE_COORD_SECTION\n") )

;;stampo i valori nella mappa
(with-open [wrtr (io/writer (str "./resources/" fileName))]
 		(.write wrtr initstr )
                ;;stampo le coordinate del nodo magazzino
                ;;(.write wrtr (str (:id store) " " (:x store) " " (:y store) "\n") )
                (doseq [cl nlist]
                  (.write wrtr (str (:id cl) " " (:x cl) " " (:y cl) "\n") ))
		)
)

