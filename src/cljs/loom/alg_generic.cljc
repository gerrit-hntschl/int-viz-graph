(ns loom.alg-generic)

(defn topsort-component
  "Topological sort of a component of a (presumably) directed graph.
  Returns nil if the graph contains any cycles. See loom.alg/topsort
  for a complete topological sort"
  ([successors start]
   (topsort-component successors start #{} #{}))
  ([successors start seen explored]
   (loop [seen seen
          explored explored
          result ()
          stack [start]]
     (if (empty? stack)
       result
       (let [v (peek stack)
             seen (conj seen v)
             us (remove explored (successors v))]
         (if (seq us)
           (when-not (some seen us)
             (recur seen explored result (conj stack (first us))))
           (recur seen (conj explored v) (conj result v) (pop stack))))))))

(defn bf-traverse
  "Traverses a graph breadth-first from start, successors being a
  function that returns adjacent nodes. When :f is provided, returns a
  lazy seq of (f node predecessor-map depth) for each node traversed.
  Otherwise, returns a lazy seq of the nodes. When :when is provided,
  filters successors with (f neighbor predecessor depth)."
  [successors start & {:keys [f when seen]}]
  (let [f (or f (fn [n p d] n))
        nbr-pred (or when (constantly true))]
    (letfn [(step [queue preds]
              (when-let [[node depth] (peek queue)]
                (cons
                  (f node preds depth)
                  (lazy-seq
                    (let [nbrs (->> (successors node)
                                    (remove #(contains? preds %))
                                    (filter #(nbr-pred % node (inc depth))))]
                      (step (into (pop queue) (for [nbr nbrs] [nbr (inc depth)]))
                            (reduce #(assoc %1 %2 node) preds nbrs)))))))]
      (step (conj #?(:clj clojure.lang.PersistentQueue/EMPTY
                     :cljs cljs.core/PersistentQueue.EMPTY) [start 0])
            (if (map? seen)
              (assoc seen start nil)
              (into {start nil} (for [s seen] [s nil])))))))
