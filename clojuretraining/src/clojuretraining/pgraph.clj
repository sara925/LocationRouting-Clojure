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
                                " [\n label = " (:id n) "\n  pos=\"" (/  (:x n) 3) "," (/ (:y n) 3) "!\"\n]\n" ))
              )
   (.write wrtr "}")
)
;;end
)
