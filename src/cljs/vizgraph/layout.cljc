(ns vizgraph.layout
  (:require [loom.graph :as graph]
            [loom.alg :as alg]
            [plumbing.graph :as pgraph]
            [clojure.pprint :as pprint]
            [plumbing.fnk.pfnk :as pfnk]
            [plumbing.core
             :refer [fnk map-vals]
             :refer-macros [fnk]]
            [clojure.set :as set]))

(defn get-node-index [layer-i-nodes neighbor]
  (->> layer-i-nodes
       (keep-indexed (fn [idx node] (when (= node neighbor)
                                      idx)))
       (first)))

(def delta-local 70)

(def delta 100)

(defn place-block [v->root v->align v->sink v->shift v->x v->inner-shift id->size predecessor-fn v]
  (when-not (get @v->x v)
    (swap! v->x assoc v 0)
    (let [initial (atom true)]
      (println "place" v "0")
      (loop [w v]
        (when-let [w-pred (predecessor-fn w)]
          (let [u (get v->root w-pred)]
            (println "pred-root u" u)
            (place-block v->root v->align v->sink v->shift v->x v->inner-shift id->size predecessor-fn u)
            (println "back to v" v ", u" u)
            (when (= (get @v->sink v) v)
              (println "new sink for " v " -> " u)
              (swap! v->sink assoc v (get @v->sink u)))
            (if (not= (get @v->sink v) (get @v->sink u))
              (let [sc (- (+ (get @v->x v) (get v->inner-shift w))
                          (get @v->x u) (get v->inner-shift w-pred) (get-in id->size [w-pred :width]) delta-local)]
                (swap! v->shift assoc (get @v->sink u) (min (get @v->shift (get @v->sink u))
                                                            sc))
                (println "new shift, sink v" v " != sink u " (get @v->sink u) " -> " (min (get @v->shift (get @v->sink u))
                                                                                          sc)))
              (let [sb (- (+ (get @v->x u) (get v->inner-shift w-pred) (get-in id->size [w-pred :width])
                             delta-local)
                          (get v->inner-shift w))]
                (if @initial
                  (swap! v->x assoc v sb)
                  (swap! v->x assoc v (max (get @v->x v) sb)))
                (reset! initial false)
                (println "new x for" v " -> " (max (get @v->x v) (+ (get @v->x u) delta-local)))))))
        (let [w-next (get v->align w)]
          (when (not= w-next v)
            (recur w-next)))))))


#_(defn place-block [v->root v->align v->sink v->shift v->x v->inner-shift id->size predecessor-fn v]
  (when-not (get @v->x v)
    (swap! v->x assoc v 0)
    (let [initial (atom true)]
      (println "place" v "0")
      (loop [w v]
        (when-let [w-pred (predecessor-fn w)]
          (let [u (get v->root w-pred)]
            (println "pred-root u" u)
            (place-block v->root v->align v->sink v->shift v->x v->inner-shift id->size predecessor-fn u)
            (println "back to v" v ", u" u)
            (when (= (get @v->sink v) v)
              (println "new sink for " v " -> " u)
              (swap! v->sink assoc v (get @v->sink u)))
            (if (not= (get @v->sink v) (get @v->sink u))
              (let [sc (- (+ (get @v->x v) (get v->inner-shift w))
                          (get @v->x u) (get v->inner-shift w-pred) (get-in id->size [w-pred :width]) delta-local)
                    sc (- (get @v->x v)
                             (get @v->x u)
                             delta)]
                (swap! v->shift assoc (get @v->sink u) (min (get @v->shift (get @v->sink u))
                                                            sc))
                (println "new shift, sink v" v " != sink u " (get @v->sink u) " -> " (min (get @v->shift (get @v->sink u))
                                                                                          sc)))
              (let [sb (- (+ (get @v->x u) (get v->inner-shift w-pred) (get-in id->size [w-pred :width])
                             delta-local)
                          (get v->inner-shift w))
                    sb (+ (get @v->x u) delta)]
                (swap! v->x assoc v (max (get @v->x v) sb))
                #_(if @initial
                  (swap! v->x assoc v sb)
                  (swap! v->x assoc v (max (get @v->x v) sb)))
                (reset! initial false)
                (println "new x for" v " -> " (max (get @v->x v) (+ (get @v->x u) delta-local)))))))
        (let [w-next (get v->align w)]
          (when (not= w-next v)
            (recur w-next)))))))

(def MAX_NUMBER_VAL #?(:clj Integer/MAX_VALUE
                       :cljs js/Infinity))

(def floor #?(:clj (fn [x] (Math/floor x))
              :cljs (fn [x] (js/Math.floor x))))

(def ceil #?(:clj (fn [x] (Math/ceil x))
              :cljs (fn [x] (js/Math.ceil x))))

(defn align-vertically [dummy-graph
                        height
                        layer-to-nodes
                        type-1-conflicts
                        direction-fn
                        layer-indices
                        next-layer-neighbors-fn
                        next-layer-index-fn]
  (let [vs (graph/nodes dummy-graph)
        v->root (atom (zipmap vs vs))
        v->align (atom (zipmap vs vs))
        layer-to-nodes (map-vals direction-fn layer-to-nodes)]
    (println "\n--------------- alignment -------------")
    (println "height" height)
    (doseq [i layer-indices]
      (println "\ni" i)
      (let [r (atom -1)]
        (doseq [vki (get layer-to-nodes i)]
          (println "\nvki" vki)
          (let [next-layer-neighbors-vki (or (next-layer-neighbors-fn dummy-graph vki)
                                        #{})
                next-layer-nodes (get layer-to-nodes (next-layer-index-fn i))
                ordered-next-layer-neighbors-vki (into []
                                                  (filter next-layer-neighbors-vki)
                                                  next-layer-nodes)
                number-next-layer-neighbors-vki (count next-layer-neighbors-vki)]
            (println "next-layer-neighbors-vki" next-layer-neighbors-vki)
            (println "number-next-layer-neighbors-vki" number-next-layer-neighbors-vki)
            (if (pos? number-next-layer-neighbors-vki)
              (doseq [m (distinct
                          ;; we have zero-based indices, so decrement
                          [(dec (int (floor (/ (inc number-next-layer-neighbors-vki) 2))))
                           (dec (int (ceil (/ (inc number-next-layer-neighbors-vki) 2))))])]
                (println "m" m)
                (when (= (get @v->align vki) vki)
                  (let [um (get ordered-next-layer-neighbors-vki m)
                        pos-um (get-node-index next-layer-nodes um)]
                    (println "um" um)
                    (println "pos[um]" pos-um)
                    (when (and (not (contains? type-1-conflicts
                                               #{um vki}))
                               (< @r pos-um))
                      (println "aligning" um vki)
                      (swap! v->align assoc um vki)
                      (swap! v->root assoc vki (get @v->root um))
                      (swap! v->align assoc vki (get @v->root vki))
                      (reset! r pos-um)
                      (println "new r" @r))))))))))
    {:root  @v->root
     :align @v->align}))

(defn horizontal-compaction [vertical-alignment dummy-graph layer-to-nodes all-node-to-layer id->size inner-shift-and-block-size]
  (let [vs (graph/nodes dummy-graph)
        v->root (:root vertical-alignment)
        v->align (:align vertical-alignment)
        v->sink (atom (zipmap vs vs))
        v->shift (atom (zipmap vs (repeat MAX_NUMBER_VAL)))
        v->inner-shift (:inner-shift inner-shift-and-block-size)
        v->x (atom {})
        predecessor-fn (fn [v]
                         (let [v-layer (get layer-to-nodes
                                            (get all-node-to-layer v))
                               v-index (get-node-index v-layer v)]
                           (println "pred v" v "v-index" v-index " -> " (when (pos? v-index)
                                                                          (get v-layer (dec v-index))))
                           (when (pos? v-index)
                             (get v-layer (dec v-index)))))]
    (doseq [v (into []
                    (filter (fn [v] (= v (get v->root v))))
                    vs)]
      (println "root-node" v)
      (place-block v->root v->align v->sink v->shift v->x v->inner-shift id->size predecessor-fn v))
    (println "- absolute coords -")
    (doseq [v vs]
      (println "coord" v ", root[v]" (get v->root v) "new x[v] = x[root[v]]" (get @v->x (get v->root v)))
      (swap! v->x assoc v (get @v->x (get v->root v)))
      (let [shift-sink-root-v (get @v->shift (get @v->sink (get v->root v)))]
        (println "shift-sink-root-v" shift-sink-root-v "root-sink" (get @v->sink (get v->root v)))
        (when (< shift-sink-root-v MAX_NUMBER_VAL)
          (println "shift v '" v "' by" shift-sink-root-v " -> " (+ (get @v->x v)
                                                                    shift-sink-root-v))
          (swap! v->x assoc v (+ (get @v->x v)
                                 shift-sink-root-v)))))
    @v->x))

(defn inner-shift [dummy-graph id->size vertical-alignment]
  (let [vs (graph/nodes dummy-graph)
        v->inner-shift (atom (zipmap vs (repeat 0)))
        v->root (:root vertical-alignment)
        v->align (:align vertical-alignment)
        v->xp (fn [v] (/ (get-in id->size [v :width]) 2))
        root->block-size (atom {})]
    (doseq [b-root (into []
                         (filter (fn [v] (= v (get v->root v))))
                         vs)]
      (let [left (atom 0)
            right (atom 0)]
        (loop [vp b-root]
          (let [vq (get v->align vp)
                s (- (+ (get @v->inner-shift vp) (v->xp vp)) (v->xp vq))]
            (swap! v->inner-shift assoc vq s)
            (swap! left min s)
            (swap! right max (+ s (get-in id->size [vq :width])))
            (when-not (= vq b-root)
              (recur vq))))
        (loop [vp b-root]
          (swap! v->inner-shift update vp - @left)
          (let [vq (get v->align vp)]
            (when-not (= vq b-root)
              (recur vq))))
        (swap! root->block-size assoc b-root (- @right @left))))
    {:inner-shift @v->inner-shift
     :block-size  @root->block-size}))

(defn adjacent-indices [next-layer neighbours v]
  {:post [(or (not-any? nil? %)
               (throw (ex-info "nil index neighbor" {:neighbors neighbours
                                                     :next-layer next-layer
                                                     :v v})))]}
  (into [] (sort (map (partial get-node-index next-layer) neighbours))))

(defn median-neighbor-index [neighbor-indices]
  {:post [(not (nil? %))]}
  (let [number-neighbors (count neighbor-indices)
        m (int (/ number-neighbors 2))]
    (cond (zero? number-neighbors)
          -1
          (= (mod number-neighbors 2) 1)
          (get neighbor-indices m)
          (= number-neighbors 2)
          (/ (+ (get neighbor-indices 0) (get neighbor-indices 1))
             2)
          :else
          (let [left (- (get neighbor-indices (dec m)) (get neighbor-indices 0))
                right (- (get neighbor-indices (dec number-neighbors)) (get neighbor-indices m))]
            (/ (+ (* (get neighbor-indices (dec m)) right)
                  (* (get neighbor-indices m) left))
               (+ left right))))))

(defn sort-with-fixed-positions [v->median-neighbor-index layer]
  {:pre [(or (every? v->median-neighbor-index layer)
             (throw (ex-info (str "node in layer without sort order:" (seq (remove v->median-neighbor-index layer)))
                             {:median-indices v->median-neighbor-index
                              :layer          layer})))]}
  (let [fixed-positions (into #{}
                              (filter (fn [v]
                                        (= -1 (get v->median-neighbor-index v))))
                              layer)
        sorted-rest (sort-by
                      v->median-neighbor-index
                      (set/difference (set layer)
                                      fixed-positions))]
    (prn "sorted-rest" sorted-rest)
    (loop [[v & vs] layer
           sorted sorted-rest
           result []]
      (if (not v)
        result
        (let [fixed? (contains? fixed-positions v)
              [next-node remaining-sorted]
              (if fixed?
                [v sorted]
                (let [next-node (first sorted)]
                  [next-node (rest sorted)]))]
          (recur vs
                 remaining-sorted
                 (conj result next-node)))))))

(defn weighted-median-sorted [ordering layer-indices next-layer-fn neighbours-fn]
  (loop [new-order {(first layer-indices) (get ordering (first layer-indices))}
         [r & next-indices] (rest layer-indices)]
    (if-not r
      new-order
      (let [next-layer (get new-order (next-layer-fn r))
            layer (get ordering r)
            v->median-neighbor-index
            (into {} (for [v layer]
                       [v (median-neighbor-index (adjacent-indices next-layer (neighbours-fn v) v))]))]
        (recur (assoc new-order r (sort-with-fixed-positions v->median-neighbor-index layer))
               next-indices)))))


(defn crossing-count [g order layer-index]
  (let [layer (get order layer-index)
        succ-layer (get order (dec layer-index))]
    (->> layer
         (reduce (fn [{:keys [prevs cross-count]} node]
                   (let [succs-indices (map (partial get-node-index succ-layer) (graph/successors g node))]
                     (when (some nil? succs-indices)
                       (throw (ex-info "successor without index"
                                       {:succs-indices succs-indices
                                        :node node
                                        :succs (graph/successors g node)})))
                     {:prevs       (reduce (fn [acc succ-index]
                                             (update acc succ-index (fnil inc 0)))
                                           prevs
                                           succs-indices)
                      :cross-count (reduce +
                                           cross-count
                                           (map (fn [succ-index]
                                                  (reduce + (keep (fn [[i occurences]]
                                                                    (when (< succ-index i)
                                                                      occurences))
                                                                  prevs)))
                                                succs-indices))}))
                 {:prevs       {}
                  :cross-count 0})
         (:cross-count))))

(defn crossing-count-for-layers [g order layers]
  (reduce + (map (partial crossing-count g order) layers)))

(defn crossing-count-involving-layer [g height order r]
  (crossing-count-for-layers g order (range (min (dec height) (inc r)) (dec r) -1)))


(defn crossing-count-for-ordering [g height order]
  (crossing-count-for-layers g order (range (dec height) 0 -1)))

(defn transpose [g height ordering]
  (let [improved (atom true)
        order (atom ordering)]
    (while @improved
      (reset! improved false)
      (doall (for [r (range 0 height)
             [v w] (partition 2 1 (get @order r))]
               (when (> (crossing-count-involving-layer g height @order r)
                        (crossing-count-involving-layer g height (update @order r (partial replace {v w, w v})) r))
                 (println "better after replacement: "
                          (crossing-count-involving-layer g height (update @order r (partial replace {v w, w v})) r)
                          "r" r
                          "[v w]" [v w])
                 (reset! improved true)
                 (swap! order update r (partial replace {v w, w v}))))))
    @order))

(defn alternating-directions [dummy-graph height i]
  (if (even? i)
    ;; the dummy-graph only has short edges
    ;; thus all neighbours are in the next/previous layer
    [(range (dec height) -1 -1) inc (partial graph/predecessors dummy-graph)]
    [(range 0 height) dec (partial graph/successors dummy-graph)]))

(defn no-real-improvement? [crossing-counts]
  (and (= 4 (count crossing-counts))
       (let [last-cc (last crossing-counts)
             improvements (map (fn [crossing-count]
                                 (- last-cc crossing-count))
                               (butlast crossing-counts))]
         (< (/ (reduce + improvements)
               last-cc)
            0.02))))

(def crossing-minimization-graph
  {:root-nodes
   (fnk [dummy-graph]
     (set/difference (graph/nodes dummy-graph)
                     (into #{} (map second) (graph/edges dummy-graph))))
   :initial-order
   (fnk [dummy-graph all-node-to-layer root-nodes]
     (->> root-nodes
          (reduce (fn [layer->nodes top-level-node]
                    (let [bf-traversal (alg/bf-traverse dummy-graph top-level-node)]
                      (reduce (fn [layer->nodes node]
                                (update layer->nodes (get all-node-to-layer node) (fnil conj []) node))
                              layer->nodes
                              bf-traversal)))
                  {})
          (map-vals (partial into [] (distinct)))))
   :ordering
   (fnk [initial-order height dummy-graph]
     (pprint/pprint {:initial-order initial-order})
     (def ii initial-order)
     (loop [i 0
            best initial-order
            [best-crossing-count :as last-crossing-counts] (list (crossing-count-for-ordering dummy-graph height initial-order))]
       (if (or (>= i 24)
               (no-real-improvement? last-crossing-counts))
         (do (println "best crossingcount:" (crossing-count-for-ordering dummy-graph height best)
                      "initial crossingcount:" (crossing-count-for-ordering dummy-graph height initial-order))
             best)
         (let [[layer-indices next-layer-fn neighbours-fn]
               (alternating-directions dummy-graph height i)
               order (weighted-median-sorted best layer-indices next-layer-fn neighbours-fn)
               _ (println "weighted order:" (crossing-count-for-ordering dummy-graph height order))
               transposed-order (transpose dummy-graph height order)
               new-crossing-count (crossing-count-for-ordering dummy-graph height transposed-order)]
           (println "new CC:" new-crossing-count)
           (if (< new-crossing-count
                  best-crossing-count)
             (recur (inc i) transposed-order (take 4 (cons new-crossing-count last-crossing-counts)))
             (recur (inc i) best (take 4 (cons best-crossing-count last-crossing-counts))))))))
   :layer-to-nodes
   (fnk [ordering initial-order]
     ;initial-order
     ordering
     )})

(def layout-graph
  {:topology-sorted                    (fnk [g]
                                         (alg/topsort g))
   :topology-sorted-reversed           (fnk [topology-sorted]
                                         (reverse topology-sorted))
   :node-to-layer                      (fnk [g topology-sorted topology-sorted-reversed]
                                         (let [layering (reduce (fn [acc node]
                                                                  (merge acc
                                                                         (into {}
                                                                               (map (fn [predecessor]
                                                                                      [predecessor
                                                                                       (max (get acc predecessor)
                                                                                            (inc (get acc node)))]))
                                                                               (graph/predecessors g node))))
                                                                (zipmap (graph/nodes g) (repeat 0))
                                                                topology-sorted-reversed)]
                                           (reduce
                                             (fn [acc node]
                                               (if-let [predecessors (seq (graph/predecessors g node))]
                                                 (assoc acc node (dec (apply min (map acc predecessors))))
                                                 acc))
                                             layering
                                             topology-sorted)))
   :edge->length                       (fnk [g node-to-layer]
                                         (into {}
                                               (map (juxt identity (fn [[src dest]]
                                                                     (- (get node-to-layer src)
                                                                        (get node-to-layer dest)))))
                                               (graph/edges g)))
   :long-edges                         (fnk [edge->length]
                                         (into #{}
                                               (keep (fn [[edge length]]
                                                       (when (> length 1)
                                                         edge)))
                                               edge->length))

   :dummies                            (fnk [long-edges node-to-layer edge->length]
                                         (->> long-edges
                                              (map (fn [[src dest :as long-edge]]
                                                     (let [dummy-nodes (map (fn [dummy-index]
                                                                              {:src         src
                                                                               :dest        dest
                                                                               :dummy-index dummy-index
                                                                               :layer       (- (get node-to-layer src) dummy-index)})
                                                                            (range 1 (get edge->length long-edge)))
                                                           dummy-edges (->> (concat [src] dummy-nodes [dest])
                                                                            (partition 2 1)
                                                                            (map vec))]
                                                       {:dummy-nodes dummy-nodes
                                                        :dummy-edges dummy-edges}))
                                                   )
                                              (apply merge-with concat)))
   :dummy-graph                        (fnk [g dummies long-edges]
                                         (-> g
                                             (graph/remove-edges* long-edges)
                                             (graph/add-nodes* (:dummy-nodes dummies))
                                             (graph/add-edges* (:dummy-edges dummies))))
   :dummy-node-to-layer                (fnk [dummies]
                                         (into {}
                                               (map (juxt identity :layer))
                                               (:dummy-nodes dummies)))
   :all-node-to-layer                  (fnk [node-to-layer dummy-node-to-layer]
                                         (merge node-to-layer dummy-node-to-layer))
   :unsorted-layer-to-nodes            (fnk [all-node-to-layer]
                                         (reduce-kv (fn [acc k v]
                                                      (update acc v (fnil conj []) k))
                                                    {}
                                                    all-node-to-layer))
   :crossing-minimized-layer-to-nodes  crossing-minimization-graph
   :layer-to-nodes                     (fnk [crossing-minimized-layer-to-nodes]
                                         (:layer-to-nodes crossing-minimized-layer-to-nodes))

   :height                             (fnk [unsorted-layer-to-nodes]
                                         (count unsorted-layer-to-nodes))
   :vertices-incident-to-inner-segment (fnk [dummies dummy-graph]
                                         ; dummy nodes have an out-degree of 1
                                         ; and the proper layering of the graph
                                         ; guarantees only short edges, so if any
                                         ; successor of a dummy vertex is a dummy
                                         ; vertex, then the vertex is incident to
                                         ; an inner segment between L_i and L_i+1
                                         (into #{}
                                               (filter (fn [dummy-vertex]
                                                         (some :dummy-index (graph/predecessors dummy-graph dummy-vertex))))
                                               (:dummy-nodes dummies)))
   :type-1-conflicts                   (fnk [height vertices-incident-to-inner-segment layer-to-nodes dummy-graph]
                                         (let [marked (atom #{})]
                                           (doseq [i (range (- height 2) 1 -1)]
                                             (println)
                                             (println "-----------------")
                                             (println "Li" i)
                                             (let [k0 (atom 0)
                                                   l 0
                                                   Li+1 (get layer-to-nodes (dec i))
                                                   cardinality-Li+1 (count Li+1)
                                                   max-index-Li+1 (dec cardinality-Li+1)]
                                               (doseq [l1 (range 0 cardinality-Li+1)]
                                                 (let [vl1Li+1 (get Li+1 l1)]
                                                   (println "vl1Li+1" vl1Li+1)
                                                   (if (or (= l1 max-index-Li+1)
                                                           (contains? vertices-incident-to-inner-segment
                                                                      vl1Li+1))
                                                     (let [k1 (if (contains? vertices-incident-to-inner-segment
                                                                             vl1Li+1)
                                                                (do (println "vl1Li+1 inner segment")
                                                                    (let [upper-neighbor (first (graph/predecessors dummy-graph vl1Li+1))]
                                                                      (get-node-index (get layer-to-nodes i)
                                                                                      upper-neighbor)))
                                                                (dec (count (get layer-to-nodes i))))]
                                                       (println "k1" k1)
                                                       (doseq [lx (range l (inc l1))]
                                                         (println "lx" lx)
                                                         (let [vli+1 (get Li+1 lx)]
                                                           (println "vli+1" vli+1)
                                                           (doseq [upper-neighbor-vki (graph/predecessors dummy-graph vli+1)]
                                                             (println "upper-neighbor-vki" upper-neighbor-vki)
                                                             (let [k (get-node-index (get layer-to-nodes i)
                                                                                     upper-neighbor-vki)]
                                                               (println "k" k)
                                                               (when (or (< k @k0) (> k k1))
                                                                 (println "marked: (< k @k0)" (< k @k0) "(> k k1)" (> k k1))
                                                                 (swap! marked conj #{upper-neighbor-vki vli+1}))))))
                                                       (reset! k0 k1)
                                                       (println "k0" @k0)))))))
                                           @marked))
   :vertical-alignment-up-left         (fnk [dummy-graph height layer-to-nodes type-1-conflicts]
                                         (println "up left align")
                                         (align-vertically dummy-graph
                                                           height
                                                           layer-to-nodes
                                                           type-1-conflicts
                                                           identity
                                                           (range (- height 2) -1 -1)
                                                           graph/predecessors
                                                           inc))
   :inner-shift-up-left                (fnk [dummy-graph id->size vertical-alignment-up-left]
                                         (inner-shift dummy-graph id->size vertical-alignment-up-left))
   :horizontal-compaction-up-left      (fnk [vertical-alignment-up-left dummy-graph layer-to-nodes all-node-to-layer id->size inner-shift-up-left]
                                         (println "\n -------------- compaction up left ------------")
                                         (horizontal-compaction vertical-alignment-up-left
                                                                dummy-graph
                                                                layer-to-nodes
                                                                all-node-to-layer
                                                                id->size
                                                                inner-shift-up-left))
   :vertical-alignment-up-right        (fnk [dummy-graph height layer-to-nodes type-1-conflicts]
                                         (align-vertically dummy-graph
                                                           height
                                                           layer-to-nodes
                                                           type-1-conflicts
                                                           (comp vec reverse)
                                                           (range (- height 2) -1 -1)
                                                           graph/predecessors
                                                           inc))
   :inner-shift-up-right               (fnk [dummy-graph id->size vertical-alignment-up-right]
                                         (inner-shift dummy-graph id->size vertical-alignment-up-right))
   :horizontal-compaction-up-right     (fnk [vertical-alignment-up-right dummy-graph layer-to-nodes all-node-to-layer id->size inner-shift-up-right]
                                         (println "\n -------------- compaction ------------")
                                         (horizontal-compaction vertical-alignment-up-right
                                                                dummy-graph
                                                                layer-to-nodes
                                                                all-node-to-layer
                                                                id->size
                                                                inner-shift-up-right))
   :vertical-alignment-down-left       (fnk [dummy-graph height layer-to-nodes type-1-conflicts]
                                         (align-vertically dummy-graph
                                                           height
                                                           layer-to-nodes
                                                           type-1-conflicts
                                                           identity
                                                           (range 0 height)
                                                           graph/successors
                                                           dec))
   :inner-shift-down-left              (fnk [dummy-graph id->size vertical-alignment-down-left]
                                         (inner-shift dummy-graph id->size vertical-alignment-down-left))
   :horizontal-compaction-down-left    (fnk [vertical-alignment-down-left dummy-graph layer-to-nodes all-node-to-layer id->size inner-shift-down-left]
                                         (println "\n -------------- compaction ------------")
                                         (horizontal-compaction vertical-alignment-down-left dummy-graph layer-to-nodes all-node-to-layer id->size inner-shift-down-left))
   :vertical-alignment-down-right      (fnk [dummy-graph height layer-to-nodes type-1-conflicts]
                                         (align-vertically dummy-graph
                                                           height
                                                           layer-to-nodes
                                                           type-1-conflicts
                                                           (comp vec reverse)
                                                           (range 0 height)
                                                           graph/successors
                                                           dec))
   :inner-shift-down-right             (fnk [dummy-graph id->size vertical-alignment-down-right]
                                         (inner-shift dummy-graph id->size vertical-alignment-down-right))
   :horizontal-compaction-down-right   (fnk [vertical-alignment-down-right dummy-graph layer-to-nodes all-node-to-layer id->size inner-shift-down-right]
                                         (println "\n -------------- compaction ------------")
                                         (horizontal-compaction vertical-alignment-down-right
                                                                dummy-graph
                                                                layer-to-nodes
                                                                all-node-to-layer
                                                                id->size
                                                                inner-shift-down-right))
   :smallest-width-alignment-lo-hi     (fnk [horizontal-compaction-up-left
                                             horizontal-compaction-up-right
                                             horizontal-compaction-down-left
                                             horizontal-compaction-down-right]
                                         (apply min-key
                                                (fn [[minv maxv]]
                                                  (- maxv minv))
                                                (map (fn [assignment]
                                                       [(apply min (vals assignment))
                                                        (apply max (vals assignment))])
                                                     [horizontal-compaction-up-left
                                                      horizontal-compaction-up-right
                                                      horizontal-compaction-down-left
                                                      horizontal-compaction-down-right])))
   :up-left-shift                      (fnk [horizontal-compaction-up-left smallest-width-alignment-lo-hi]
                                         (- (apply min (vals horizontal-compaction-up-left))
                                            (first smallest-width-alignment-lo-hi)))
   :down-left-shift                    (fnk [horizontal-compaction-down-left smallest-width-alignment-lo-hi]
                                         (- (apply min (vals horizontal-compaction-down-left))
                                            (first smallest-width-alignment-lo-hi)))
   :up-right-shift                     (fnk [horizontal-compaction-up-right smallest-width-alignment-lo-hi]
                                         (- (second smallest-width-alignment-lo-hi)
                                            (apply max (vals horizontal-compaction-up-right))))
   :down-right-shift                   (fnk [horizontal-compaction-down-right smallest-width-alignment-lo-hi]
                                         (- (second smallest-width-alignment-lo-hi)
                                            (apply max (vals horizontal-compaction-down-right))))
   :up-left-aligned                    (fnk [horizontal-compaction-up-left up-left-shift]
                                         (map-vals (fn [x] (+ x up-left-shift))
                                                   horizontal-compaction-up-left))
   :down-left-aligned                  (fnk [horizontal-compaction-down-left down-left-shift]
                                         (map-vals (fn [x] (+ x down-left-shift))
                                                   horizontal-compaction-down-left))
   :up-right-aligned                   (fnk [horizontal-compaction-up-right up-right-shift]
                                         (map-vals (fn [x] (+ x up-right-shift))
                                                   horizontal-compaction-up-right))
   :down-right-aligned                 (fnk [horizontal-compaction-down-right down-right-shift]
                                         (map-vals (fn [x] (+ x down-right-shift))
                                                   horizontal-compaction-down-right))
   :layer-y-coordinates                (fnk [layer-to-nodes id->size]
                                         (let [sorted-layer-indices (sort-by - (keys layer-to-nodes))]
                                           (->> sorted-layer-indices
                                                (reductions (fn [[_ last-height-sum] li]
                                                              (let [max-node-height-li (apply max
                                                                                              (map (fn [node]
                                                                                                     (get-in id->size [node :height] 0))
                                                                                                   (get layer-to-nodes li)))]
                                                                [(dec li) (+ last-height-sum max-node-height-li 40)]))
                                                            [(first sorted-layer-indices) 10])
                                                (into {}))))
   :y-coordinates                      (fnk [layer-y-coordinates layer-to-nodes]
                                         (into {}
                                               (mapcat
                                                 (fn [[li offset]]
                                                   (map (fn [node] [node offset])
                                                        (get layer-to-nodes li)))
                                                 layer-y-coordinates)))
   :xy-coordinates-centered            (fnk [up-left-aligned
                                             down-left-aligned
                                             up-right-aligned
                                             down-right-aligned
                                             y-coordinates]
                                         #_(into {}
                                                 (map (fn [[n x]]

                                                        [n {:x x
                                                            :y (get y-coordinates n)}])
                                                      up-right-aligned))
                                         (into {}
                                               (map (fn [[n x]]
                                                      (let [[_ x1 x2 _] (sort (cons x
                                                                                    (map (fn [alignment]
                                                                                           (get alignment n))
                                                                                         [down-left-aligned
                                                                                          up-right-aligned
                                                                                          down-right-aligned])))]
                                                        [n {:x (/ (+ x1 x2) 2)
                                                            :y (get y-coordinates n)}]))
                                                    up-left-aligned)))
   :min-x-coordinate                   (fnk [xy-coordinates-centered]
                                         (->> xy-coordinates-centered
                                              (vals)
                                              (map :x)
                                              (apply min)))
   :xy-coordinates                     (fnk [xy-coordinates-centered min-x-coordinate]
                                         (if (< min-x-coordinate 0)
                                           (map-vals (fn [xy] (update xy :x - min-x-coordinate))
                                                     xy-coordinates-centered)
                                           xy-coordinates-centered))
   :edge->src->dest                    (fnk [dummy-graph]
                                         (reduce (fn [acc [src dest]]
                                                   (assoc-in
                                                     acc
                                                     [[(if (map? src)
                                                         (:src src)
                                                         src)
                                                       (if (map? dest)
                                                         (:dest dest)
                                                         dest)]
                                                      src]
                                                     dest))
                                                 {}
                                                 (graph/edges dummy-graph)))
   :edge->node-coordinates             (fnk [id->size edge->src->dest xy-coordinates]
                                         (into {}
                                               (map (fn [[[src dest :as edge] src->dest]]
                                                      (let [src-point
                                                            {:x (+ (get-in xy-coordinates [src :x])
                                                                   (/ (get-in id->size [src :width]) 2))
                                                             :y (+ (get-in xy-coordinates [src :y])
                                                                   (get-in id->size [src :height]))}]
                                                        [edge (loop [node (get src->dest src)
                                                                     points [src-point]]
                                                                (if (= node dest)
                                                                  (conj points
                                                                        {:x (+ (get-in xy-coordinates [dest :x])
                                                                               (/ (get-in id->size [dest :width]) 2))
                                                                         :y (get-in xy-coordinates [dest :y])})
                                                                  (recur (get src->dest node)
                                                                         (conj points
                                                                               {:x (get-in xy-coordinates [node :x])
                                                                                :y (get-in xy-coordinates [node :y])}))))])))
                                               edge->src->dest))
   :size                               (fnk [xy-coordinates]
                                         {:width  (->> xy-coordinates (vals) (map :x) (apply max))
                                          :height (->> xy-coordinates (vals) (map :y) (apply max))})})

(defn handle-exi [exi m k im]
  (let [data (-> (ex-data exi)
                 (assoc :exception-during k)
                 (assoc :input-params (dissoc im :jira))
                 (assoc :graph (dissoc m :jira)))]
    (def bb data)
    (throw (ex-info #?(:clj (.getMessage exi)
                       :cljs (.-message exi))
                    data
                    exi))))

(defn compile-cancelling [g]
  (pgraph/simple-hierarchical-compile
    g
    true
    (fn [m] m)
    (fn [m k f]
      (let [im (select-keys m (pfnk/input-schema-keys f))]
        (assoc m k (try (f im)
                        (catch #?(:clj Throwable
                                  :cljs js/Error) ex
                          (def oex ex)
                          (handle-exi ex m k im))))))))

(defn block [node alignment]
  (loop [v node
         block #{}]
    (if (contains? block v)
      block
      (let [succ (get alignment v)]
        (recur succ (conj block v))))))

(defn blocks [alignment]
  (loop [nodes (set (keys alignment))
         blocks #{}]
    (if (empty? nodes)
      blocks
      (let [n (first nodes)
            b (block n alignment)]
        (recur (set/difference nodes b) (conj blocks b))))))

(def lay (compile-cancelling layout-graph))

(defn hierarchical [id->size edges]
  (let [lg (lay {:g (apply graph/digraph edges)
                 :id->size id->size})]
    (select-keys lg [:size :xy-coordinates :edge->node-coordinates])))