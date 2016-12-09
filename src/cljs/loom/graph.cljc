(ns ^{:doc "Defines protocols for graphs, digraphs, and weighted graphs.
Also provides record implementations and constructors for simple graphs --
weighted, unweighted, directed, and undirected. The implementations are based
on adjacency lists."
      :author "Justin Kramer"}
loom.graph)

;;;
;;; Protocols
;;;

(defprotocol Graph
  (nodes [g] "Returns a collection of the nodes in graph g")
  (edges [g] "Edges in g. May return each edge twice in an undirected graph")
  (has-node? [g node] "Returns true when node is in g")
  (has-edge? [g n1 n2] "Returns true when edge [n1 n2] is in g")
  (successors* [g node] "Returns direct successors of node")
  (out-degree [g node] "Returns the number of outgoing edges of node")
  (out-edges [g node] "Returns all the outgoing edges of node"))

(defprotocol Digraph
  (predecessors* [g node] "Returns direct predecessors of node")
  (in-degree [g node] "Returns the number of direct predecessors to node")
  (in-edges [g node] "Returns all the incoming edges of node")
  (transpose [g] "Returns a graph with all edges reversed"))

(defprotocol WeightedGraph
  (weight* [g e] [g n1 n2] "Returns the weight of edge e or edge [n1 n2]"))

(defprotocol EditableGraph
  (add-nodes* [g nodes] "Add nodes to graph g. See add-nodes")
  (add-edges* [g edges] "Add edges to graph g. See add-edges")
  (remove-nodes* [g nodes] "Remove nodes from graph g. See remove-nodes")
  (remove-edges* [g edges] "Removes edges from graph g. See remove-edges")
  (remove-all [g] "Removes all nodes and edges from graph g"))

(defprotocol Edge
  (src [edge] "Returns the source node of the edge")
  (dest [edge] "Returns the dest node of the edge"))

; Default implementation for vectors
#?(:cljs (extend-type PersistentVector
   Edge
   (src [edge] (get edge 0))
   (dest [edge] (get edge 1))))

; Default implementation for maps
#?(:clj
   (extend-type clojure.lang.IPersistentMap
     Edge
     (src [edge] (:src edge))
     (dest [edge] (:dest edge))))

#?(:cljs
   (extend-type PersistentArrayMap
     Edge
     (src [edge] (:src edge))
     (dest [edge] (:dest edge))))
#?(:cljs (extend-type PersistentHashMap
         Edge
         (src [edge] (:src edge))
         (dest [edge] (:dest edge))))

#?(:cljs (extend-type PersistentTreeMap
   Edge
   (src [edge] (:src edge))
   (dest [edge] (:dest edge))))

;; Curried wrappers
(defn successors
  "Returns direct successors of node"
  ([g] #(successors g %)) ; faster than partial
  ([g node] (successors* g node)))

(defn predecessors
  "Returns direct predecessors of node"
  ([g] #(predecessors g %))
  ([g node] (predecessors* g node)))

(defn weight
  "Returns the weight of edge e or edge [n1 n2]"
  ([g] (partial weight g))
  ([g e] (weight* g (src e) (dest e)))
  ([g n1 n2] (weight* g n1 n2)))

;; Variadic wrappers

(defn add-nodes
  "Adds nodes to graph g. Nodes can be any type of object"
  [g & nodes]
  (add-nodes* g nodes))

(defn add-edges
  "Adds edges to graph g. For unweighted graphs, edges take the form [n1 n2].
  For weighted graphs, edges take the form [n1 n2 weight] or [n1 n2], the
  latter defaulting to a weight of 1"
  [g & edges]
  (add-edges* g edges))

(defn remove-nodes
  "Removes nodes from graph g"
  [g & nodes]
  (remove-nodes* g nodes))

(defn remove-edges
  "Removes edges from graph g. Do not include weights"
  [g & edges]
  (remove-edges* g edges))

;;;
;;; Records for basic graphs -- one edge per vertex pair/direction,
;;; loops allowed
;;;
;; TODO: allow custom weight fn?
;; TODO: preserve metadata?
;; TODO: leverage zippers for faster record updates?

(defrecord BasicEditableDigraph [nodeset adj in])

(defn- remove-adj-nodes [m nodes adjacents remove-fn]
  (reduce
    (fn [m n]
      (if (m n)
        (update-in m [n] #(apply remove-fn % nodes))
        m))
    (apply dissoc m nodes)
    adjacents))

(extend-type BasicEditableDigraph
  Graph
  (nodes [g]
    (:nodeset g))
  (edges [g]
    (for [n1 (nodes g)
          e (out-edges g n1)]
      e))
  (has-node? [g node]
    (contains? (:nodeset g) node))
  (has-edge? [g n1 n2]
    (contains? (get-in g [:adj n1]) n2))
  (out-degree [g node]
    (count (get-in g [:adj node])))
  (out-edges
    [g node] (for [n2 (successors g node)] [node n2]))

  (successors* [g node]
    (get-in g [:adj node]))

  EditableGraph
  (add-nodes* [g nodes]
    (reduce
      (fn [g n]
        (-> g
            (update-in [:nodeset] conj n)
            (assoc-in [:adj n] (or ((:adj g) n) #{}))))
      g nodes))
  #_(add-nodes* [g nodes]
    (reduce
      (fn [g node] (update-in g [:nodeset] conj node))
      g nodes))

  (add-edges* [g edges]
    (reduce
      (fn [g [n1 n2]]
        (-> g
            (update-in [:nodeset] conj n1 n2)
            (update-in [:adj n1] (fnil conj #{}) n2)
            (update-in [:in n2] (fnil conj #{}) n1)))
      g edges))

  (remove-nodes* [g nodes]
    (let [ins (mapcat #(predecessors g %) nodes)
          outs (mapcat #(successors g %) nodes)]
      (-> g
          (update-in [:nodeset] #(apply disj % nodes))
          (assoc :adj (remove-adj-nodes (:adj g) nodes ins disj))
          (assoc :in (remove-adj-nodes (:in g) nodes outs disj)))))

  (remove-edges* [g edges]
    (reduce
      (fn [g [n1 n2]]
        (-> g
            (update-in [:adj n1] disj n2)
            (update-in [:in n2] disj n1)))
      g edges))

  (remove-all [g]
    (assoc g :nodeset #{} :adj {} :in {}))

  Digraph
  (predecessors* [g node]
    (get-in g [:in node]))
  (in-degree [g node]
    (count (get-in g [:in node])))
  (in-edges [g node]
    (for [n2 (predecessors g node)] [n2 node]))
  (transpose [g]
    (assoc g :adj (:in g) :in (:adj g))))


;;;


;;;
;;; Utility functions and constructors
;;;

;; TODO: make this work with read-only graphs?
;; Could also gain speed being impl-specific
(defn subgraph
  "Returns a graph with only the given nodes"
  [g ns]
  (remove-nodes* g (remove (set ns) (nodes g))))

(defn add-path
  "Adds a path of edges connecting the given nodes in order"
  [g & nodes]
  (add-edges* g (partition 2 1 nodes)))

(defn add-cycle
  "Adds a cycle of edges connecting the given nodes in order"
  [g & nodes]
  (add-edges* g (partition 2 1 (concat nodes [(first nodes)]))))

(defn graph?
  "Returns true if g satisfies the Graph protocol"
  [g]
  (satisfies? Graph g))

(defn directed?
  "Returns true if g satisfies the Digraph protocol"
  [g]
  (satisfies? Digraph g))

(defn weighted?
  "Returns true if g satisfies the WeightedGraph protocol"
  [g]
  (satisfies? WeightedGraph g))

(defn editable?
  "Returns true if g satisfies the EditableGraph protocol"
  [g]
  (satisfies? EditableGraph g))

(defn build-graph
  "Builds up a graph (i.e. adds edges and nodes) from any combination of
  other graphs, adjacency maps, edges, or nodes."
  [g & inits]
  (letfn [(build [g init]
            (cond
              ;; graph
              (graph? init)
              (if (and (weighted? g) (weighted? init))
                (assoc
                  (reduce add-edges
                          (add-nodes* g (nodes init))
                          (for [[n1 n2] (edges init)]
                            [n1 n2 (weight init n1 n2)]))
                  :attrs (merge (:attrs g) (:attrs init)))
                (-> g
                    (add-nodes* (nodes init))
                    (add-edges* (edges init))
                    (assoc :attrs (merge (:attrs g) (:attrs init)))))
              ;; adacency map
              (map? init)
              (let [es (if (map? (val (first init)))
                         (for [[n nbrs] init
                               [nbr wt] nbrs]
                           [n nbr wt])
                         (for [[n nbrs] init
                               nbr nbrs]
                           [n nbr]))]
                (-> g
                    (add-nodes* (keys init))
                    (add-edges* es)))
              ;; edge
              (sequential? init) (add-edges g init)
              ;; node
              :else (add-nodes g init)))]
    (reduce build g inits)))

(defn digraph
  "Creates an unweighted, directed graph. inits can be edges, adjacency maps,
  or graphs"
  [& inits]
  (apply build-graph (BasicEditableDigraph. #{} {} {}) inits))



