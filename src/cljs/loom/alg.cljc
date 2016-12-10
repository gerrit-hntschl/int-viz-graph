(ns ^{:doc "Graph algorithms. Any graph record/type that satisfies the
Graph, Digraph, or WeightedGraph protocols (as appropriate per algorithm)
can use these functions."
      :author "Justin Kramer"}
loom.alg
  (:require [loom.graph :as graph]
            [loom.alg-generic :as gen]))

(defn topsort
  "Topological sort of a directed acyclic graph (DAG). Returns nil if
  g contains any cycles."
  ([g]
   (loop [seen #{}
          result ()
          [n & ns] (seq (graph/nodes g))]
     (if-not n
       result
       (if (seen n)
         (recur seen result ns)
         (when-let [cresult (gen/topsort-component
                              (graph/successors g) n seen seen)]
           (recur (into seen cresult) (concat cresult result) ns))))))
  ([g start]
   (gen/topsort-component (graph/successors g) start)))


(defn bf-traverse
  "Traverses graph g breadth-first from start. When option :f is provided,
  returns a lazy seq of (f node predecessor-map depth) for each node traversed.
  Otherwise, returns a lazy seq of the nodes. When option :when is provided,
  filters successors with (f neighbor predecessor depth)."
  ([g]
   (first
     (reduce
       (fn [[cc predmap] n]
         (if (contains? predmap n)
           [cc predmap]
           (reduce
             (fn [[cc _] [n pm _]]
               [(conj cc n) pm])
             [cc predmap]
             (gen/bf-traverse (graph/successors g) n :f vector :seen predmap))))
       [[] {}]
       (graph/nodes g))))
  ([g start]
   (gen/bf-traverse (graph/successors g) start))
  ([g start & opts]
   (apply gen/bf-traverse (graph/successors g) start opts)))