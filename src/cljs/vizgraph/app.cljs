(ns vizgraph.app
  (:import [goog.dom query])
  (:require [rum.core :as rum]
            [cljs.pprint :as pprint]
            [clojure.string :as str]
            [loom.graph :as graph]
            [cljs.reader :as reader]
            [schema.core :as schema]
            [vizgraph.layout :as layout]
            [plumbing.core :refer [map-vals]]
            [plumbing.fnk.pfnk :as pfnk]
            [clojure.set :as set]

            [goog.events :as gevents]
            )
  (:require-macros [vizgraph.core :refer [fnk]]))

(extend-type js/NodeList
  ISeqable
  (-seq [array] (array-seq array 0)))


(enable-console-print!)

(def layout-result (atom nil))

(def selection-layout (atom nil))

(def sized {:did-mount (fn [state]
                         (let [component (:rum/react-component state)
                               dom-node (rum/dom-node state)
                               [size-atom comp-name] (:rum/args state)
                               size {:width  (.-offsetWidth dom-node) ;(.-scrollWidth dom-node)
                                     :height (.-offsetHeight dom-node) ;(.-scrollHeight dom-node)
                                     }]
                           (rum/request-render component)
                           (swap! size-atom assoc comp-name size)
                           (assoc state ::size size)))})

(rum/defcs label < sized

  [state size-atom text]
  (println "render label" (::size state))
  [:div {:style (cond-> (merge {:border    "1px solid"
                                :borderRadius "5px"
                                :max-width "300px"
                                :min-width "40px"}
                               (::size state))
                        (::size state)
                        (dissoc :max-width)
                        )}
   [:p text]
   ])

(defn formatted-code [source input-keys]
  (let [input-names (into #{}
                          (map (fn [input-key]
                                 (-> input-key name (.replace \- \u2011))))
                          input-keys)
        formatted-code (when (string? source)
                         (-> (reader/read-string source)
                            (pprint/write
                              :dispatch pprint/code-dispatch)
                            (with-out-str)
                            (.replace \- \u2011)))
        code-with-marked-input (when formatted-code
                                 (reduce (fn [code input-name]
                                          (-> code
                                              (str/replace input-name (str "@@@" input-name "@@@"))))
                                        formatted-code
                                        input-names))
        code-markup (some-> code-with-marked-input
                        (str/split #"@@@")
                        (->> (map (fn [s]
                                    (if (contains? input-names s)
                                      [:span {:class "input-param"} s]
                                      [:span s])))))]
    [:div {:class "node-code"}
     [:pre
      (into [:code {:class "clojure"
                    :style {:padding "0.5em"
                            :display "block"}}]
            code-markup)]]))

(rum/defcs code-node < sized                                ;keyed
  [state size-atom node-name keywordfn]
  [:div {:style (cond-> (merge {:border    "1px solid"
                                :backgroundColor "white"
                                :borderRadius "5px"
                                :maxWidth "500px"
                                :minWidth "40px"}
                               (::size state))
                        (::size state)
                        (dissoc :maxWidth))}
   [:div.node-header
    {:style {:text-align "center"
             :border-bottom "1px solid black"}}
    node-name]
   (formatted-code (:source (meta keywordfn)) (pfnk/input-schema-keys keywordfn))
   ])

(rum/defcs input-node < sized                                ;keyed
  [state size-atom node-name]
  [:div {:style (cond-> (merge {:border    "1px solid"
                                :borderRadius "5px"
                                :maxWidth "500px"
                                :minWidth "40px"}
                               (::size state))
                        (::size state)
                        (dissoc :maxWidth))}
   [:div.node-header
    {:style {:text-align "center"
             :border-bottom "1px solid black"
             :background-color "lightgreen"}}
    node-name]])


(rum/defc positioned-node < rum/reactive
  [id view node-coords-overrides child-coordinates result]
  (let [overriden-coords (some-> (get node-coords-overrides id) (rum/react))]
    [:div {:style   {:top        (or
                                   (:y overriden-coords)
                                   (get-in child-coordinates [id :y]))
                     :position   "absolute"
                     :left       (or

                                   (:x overriden-coords)
                                   (get-in child-coordinates [id :x]))
                     :transition "top 700ms, left 700ms"}
           :class   "graph-node"
           :onClick (fn [e]
                      (println "clicked:" id "- result:" result)
                      (.preventDefault e)
                      (.stopPropagation e)
                      (reset! selection-layout (layout/selection-dependent-coords result id)
                              #_(doseq [succ succs-selected]
                                  (when (:layer succ)
                                    (let [dest (:dest succ)
                                          coords (get xy-coordinates succ)]
                                      (reset! (get node-coords-overrides dest) coords))))))}

     view]))

(rum/defcs layout < rum/reactive
                    {:did-mount (fn [state]
                                  (println "layout did-mount")
                                  (let [[size-atom nodes edges] (:rum/args state)
                                        {:keys [xy-coordinates edge->node-coordinates size dummy-graph] :as result} (layout/hierarchical @size-atom edges)]
                                    (reset! layout-result result)
                                    ;(rum/request-render (:rum/react-component state))
                                    (assoc state ::node-coords-overrides (into {}
                                                                               (map (fn [node]
                                                                                      [node (atom nil)]))
                                                                               (graph/nodes dummy-graph)))
                                    #_(assoc state ::id->coordinates xy-coordinates
                                                   ::all-edges edge->node-coordinates
                                                   ::graph-size size
                                                   ::layout-result result)))}
  [state size-atom nodes edges]
  (println "in layout")
  (let [original-layout (rum/react layout-result)
        result (or (rum/react selection-layout)
                   original-layout)
        child-coordinates (or (:xy-coordinates result)
                              (map #(update % 0 str) (iterate #(update % 0 inc) [0 0])))
        node-coords-overrides (::node-coords-overrides state)]
    [:div {:style {:position "relative"}
           :onClick (fn [e]
                      (println "resetting layout")
                      (.preventDefault e)
                      (.stopPropagation e)
                      (reset! selection-layout nil))}
     [:svg (or (map-vals (partial + 150) (:size result)) {})
      [:defs
       [:marker {:id "arrow"
                 :markerWidth "14"
                 :markerHeight "10"
                 :markerUnits "strokeWidth"
                 :viewBox "0 0 10 10"
                 :preserveAspectRatio "xMidYMid meet"
                 :refX "10"
                 :refY "5"
                 :orient "auto"}
        [:polyline {:points "0,0 10,5 0,10 1,5"}]]]
      (map (fn [edge]
             (println "edge" edge)
             (let [node-coordinates (get (:edge->node-coordinates result) edge)]
               (when (seq node-coordinates)
                 #_[:polyline {:key        (str edge)
                             :id         (str edge)
                             :marker-end "url(#arrow)"
                             :fill       "none"
                             :stroke     "black"
                             :points     (str/join " " (map (fn [{:keys [x y]}] (str x "," y))
                                                            node-coordinates))
                             :style      {:stroke       "#000000"
                                          :stroke-width 1}}]
                 [:path {:d          (str "M" (str/join "," ((juxt :x :y) (first node-coordinates)))
                                          " C" (str/join ", " ((juxt :x :y) ((if (< (count node-coordinates) 4)
                                                                               first
                                                                               second)
                                                                              node-coordinates)))
                                          " " (str/join " " (map (fn [{:keys [x y]}] (str x "," y))
                                                                 (take-last 2 node-coordinates))))
                         :fill       "transparent"
                         :stroke     "black"
                         :marker-end "url(#arrow)"
                         :key        (str edge)
                         :id         (str edge)}])))
           edges)]
     [:div
      (map (fn [{:node/keys [view id]}]
             (rum/with-key (positioned-node id view node-coords-overrides child-coordinates original-layout) id))
           nodes)]]))

#_(def example-graph {:topology-sorted                    (fnk [g]
                                                          (alg/topsort g))
                    :topology-sorted-reversed           (fnk [topology-sorted]
                                                          (reverse topology-sorted))
                    :node-to-layer                      (fnk [g topology-sorted-reversed]
                                                          (reduce (fn [acc node]
                                                                    (merge acc
                                                                           (into {}
                                                                                 (map (fn [predecessor]
                                                                                        [predecessor
                                                                                         (max (get acc predecessor)
                                                                                              (inc (get acc node)))]))
                                                                                 (graph/predecessors g node))))
                                                                  (zipmap (graph/nodes g) (repeat 0))
                                                                  topology-sorted-reversed))
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
                    :layer-to-nodes                     (fnk [unsorted-layer-to-nodes]
                                                          (map-vals (fn [nodes]
                                                                      (vec (sort-by (fn [x] (if (map? x)
                                                                                              [(:src x) (:dest x)]
                                                                                              [x nil]))
                                                                                    nodes)))
                                                                    unsorted-layer-to-nodes))
                    :height                             (fnk [layer-to-nodes]
                                                          (count layer-to-nodes))
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
                                                                                  (swap! marked conj [upper-neighbor-vki vli+1]))))))
                                                                        (reset! k0 k1)
                                                                        (println "k0" @k0)))))))
                                                            @marked))
                    :vertical-alignment-up-left         (fnk [dummy-graph height layer-to-nodes type-1-conflicts]
                                                          (align-vertically dummy-graph
                                                                            height
                                                                            layer-to-nodes
                                                                            type-1-conflicts
                                                                            identity
                                                                            (range (- height 2) -1 -1)
                                                                            graph/predecessors
                                                                            inc))

                    :horizontal-compaction-up-left      (fnk [vertical-alignment-up-left dummy-graph layer-to-nodes all-node-to-layer]
                                                          (println "\n -------------- compaction ------------")
                                                          (horizontal-compaction vertical-alignment-up-left dummy-graph layer-to-nodes all-node-to-layer))
                    :vertical-alignment-up-right        (fnk [dummy-graph height layer-to-nodes type-1-conflicts]
                                                          (align-vertically dummy-graph
                                                                            height
                                                                            layer-to-nodes
                                                                            type-1-conflicts
                                                                            (comp vec reverse)
                                                                            (range (- height 2) -1 -1)
                                                                            graph/predecessors
                                                                            inc))

                    :horizontal-compaction-up-right     (fnk [vertical-alignment-up-right dummy-graph layer-to-nodes all-node-to-layer]
                                                          (println "\n -------------- compaction ------------")
                                                          (horizontal-compaction vertical-alignment-up-right
                                                                                 dummy-graph
                                                                                 layer-to-nodes
                                                                                 all-node-to-layer))
                    :vertical-alignment-down-left       (fnk [dummy-graph height layer-to-nodes type-1-conflicts]
                                                          (align-vertically dummy-graph
                                                                            height
                                                                            layer-to-nodes
                                                                            type-1-conflicts
                                                                            identity
                                                                            (range 0 height)
                                                                            graph/successors
                                                                            dec))

                    :horizontal-compaction-down-left    (fnk [vertical-alignment-down-left dummy-graph layer-to-nodes all-node-to-layer]
                                                          (println "\n -------------- compaction ------------")
                                                          (horizontal-compaction vertical-alignment-down-left dummy-graph layer-to-nodes all-node-to-layer))
                    :vertical-alignment-down-right      (fnk [dummy-graph height layer-to-nodes type-1-conflicts]
                                                          (align-vertically dummy-graph
                                                                            height
                                                                            layer-to-nodes
                                                                            type-1-conflicts
                                                                            (comp vec reverse)
                                                                            (range 0 height)
                                                                            graph/successors
                                                                            dec))

                    :horizontal-compaction-down-right   (fnk [vertical-alignment-down-right dummy-graph layer-to-nodes all-node-to-layer]
                                                          (println "\n -------------- compaction ------------")
                                                          (horizontal-compaction vertical-alignment-down-right
                                                                                 dummy-graph
                                                                                 layer-to-nodes
                                                                                 all-node-to-layer))
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
                    :y-coordinates                      (fnk [layer-to-nodes]
                                                          (let [sorted-layers (sort-by - (keys layer-to-nodes))
                                                                offsets (range 0 (* (count sorted-layers) 70) 70)]
                                                            (into {}
                                                                  (mapcat (fn [l offset]
                                                                            (map (fn [n] [n offset]) (get layer-to-nodes l)))
                                                                          sorted-layers offsets))))
                    :xy-coordinates-centered            (fnk [up-left-aligned
                                                              down-left-aligned
                                                              up-right-aligned
                                                              down-right-aligned
                                                              y-coordinates]
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
                                                          (if (> min-x-coordinate 0)
                                                            (map-vals (partial - min-x-coordinate)
                                                                      xy-coordinates-centered)
                                                            xy-coordinates-centered))
                    :edge->src->dest                    (fnk [dummy-graph]
                                                          (println "esd")
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
                                                          (println "enc")
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

(def missing-users-graph
  ;; input -> import-usernames-all & dbval & jira
  {:db-usernames-all                 (fnk [dbval]
                                       )
   :import-usernames-new           (fnk [import-usernames-all db-usernames-all]
                                     )
   :jira-users-new                   (fnk [jira import-usernames-new]
                                       )
   :domain-users-new                 (fnk [jira-users-new]
                                       )})

(def example-graph
  ;; input:
  ;; dbval :- Datomic database value,
  ;; today :- Date of today with time 1 millisecond before tomorrow
  ;; jira :- Jira Client implementation
  (merge
    {:re-import?           (fnk [dbval]
                             )
     :current-period-start (fnk [today]
                             )
     :import-start-date    (fnk [re-import? today current-period-start]
                             )
     :import-end-date      (fnk [today]
                             )
     :worklogs-retrieved   (fnk [jira import-start-date import-end-date]
                             ;; returns a map containing all intermediate state used to retrieve actual worklogs
                             ;; other entries are not used, but useful for debugging
                             )
     :worklogs-all         (fnk worklogs-all
                             [worklogs-retrieved]
                             ;; simplify access to actual worklogs
                             )
     :import-usernames-all (fnk [worklogs-all]
                             )}
    missing-users-graph
    {:jira-usernames-new               (fnk [jira-users-new]
                                         (into #{} (map :name) jira-users-new))
     :worklogs-usernames-unknown       (fnk [jira-usernames-new import-usernames-new]
                                         )
     :issue-ids-all                    (fnk [worklogs-all]
                                         )
     :db-ticket-ids-all                (fnk [dbval]
                                         )
     :issue-ids-new                    (fnk [issue-ids-all db-ticket-ids-all]
                                         )
     :issues-new-parsed-json           (fnk [jira issue-ids-new]
                                         )
     :issues-new-coerced               (fnk [issues-new-parsed-json]
                                         )
     :tickets-new                      (fnk [issues-new-coerced]
                                         )
     :db-customer-ids-all              (fnk [dbval]
                                         )
     ;; can the component of a jira issue change? here we assume that only new issues can introduce new components
     :jira-customers-new               (fnk [issues-new-coerced]
                                         )
     :jira-customer-ids                (fnk [jira-customers-new]
                                         )
     :customer-ids-new                 (fnk [db-customer-ids-all jira-customer-ids]
                                         )
     :domain-customers-new             (fnk [customer-ids-new jira-customers-new]
                                         )
     :worklogs-known-usernames         (fnk [worklogs-all worklogs-usernames-unknown]
                                         )
     :db-worklog-ids-in-import-range   (fnk [re-import? dbval import-start-date import-end-date]
                                         )
     :jira-worklog-ids-in-import-range (fnk [worklogs-known-usernames]
                                         )
     :jira-worklog-ids-deleted         (fnk [re-import? db-worklog-ids-in-import-range jira-worklog-ids-in-import-range]
                                         )
     :domain-worklogs-new              (fnk [worklogs-known-usernames]
                                         )
     :domain-worklogs-deleted          (fnk [jira-worklog-ids-deleted]
                                         )
     :domain-worklogs-transaction      (fnk [domain-worklogs-new domain-worklogs-deleted]
                                         )
     :db-transactions                  (fnk [domain-users-new domain-customers-new tickets-new domain-worklogs-transaction]
                                         )
     :db-transactions-stats            (fnk [domain-users-new domain-customers-new tickets-new domain-worklogs-deleted domain-worklogs-new]
                                         )}))

#_(def example-graph {:x             (fnk [a]
                                     (+ a 5))
                    :y             (fnk [x a]
                                     (* x a))
                    :z             (fnk [x y b]
                                     (vector x y b))
                    :v             (fnk [z b y]
                                     (println (/ z b y)
                                              "faskldjfkalsjdfklajskldfjlk"))
                    :h             (fnk [a z]
                                     (* a z))
                    :node-to-layer (fnk [g topology-sorted-reversed]
                                     (reduce (fn [acc node]
                                               (merge acc
                                                      (into {}
                                                            (map (fn [predecessor]
                                                                   [predecessor
                                                                    (max (get acc predecessor)
                                                                         (inc (get acc node)))]))
                                                            (graph/predecessors g node))))
                                             (zipmap (graph/nodes g) (repeat 0))
                                             topology-sorted-reversed))
                    :i             (fnk [h v node-to-layer]
                                     (do node-to-layer
                                         (range h v)))})

#_(def example-graph {:e (fnk [b] (* 2 b))
                    :f (fnk [a] (* 3 a))
                    :x (fnk [e f] (/ e f))})


#_(def example-graph {:a03 (fnk [x01 x02])
                    :x04 (fnk [x01 a03])
                    :x05 (fnk [a03])
                    :x06 (fnk [x04])
                    :x07 (fnk [x02 x05])
                    ;:x08 (fnk [x06])
                    ;:x09 (fnk [x07])
                    ;:x10 (fnk [x08])
                    ;:x11 (fnk [x08])
                    ;:x12 (fnk [x09])
                    ;:x13 (fnk [x01 x10])
                    ;:x14 (fnk [x10])
                    ;:x15 (fnk [x10 x11])
                    ;:x16 (fnk [x06 x11])
                    ;:x17 (fnk [x13 x14])
                    ;:x18 (fnk [x14 x16])
                    ;:x19 (fnk [x16])
                    ;:x20 (fnk [x12 x16 x02])
                    ;:x21 (fnk [x01 x18])
                    ;:x22 (fnk [x19])
                    ;:x23 (fnk [x03 x06 x21 x22])
                    })

#_(def example-graph {:x03 (fnk [x01 x02])
                    :x04 (fnk [x01 x03])
                    :x05 (fnk [x03])
                    :x06 (fnk [x04])
                    :x07 (fnk [x05])
                    :x08 (fnk [x06])
                    :x09 (fnk [x07])
                    :x10 (fnk [x08])
                    :x11 (fnk [x08])
                    :x12 (fnk [x09])
                    :x13 (fnk [x01 x10])
                    :x14 (fnk [x10])
                    :x15 (fnk [x10 x11])
                    :x16 (fnk [x06 x11])
                    :x17 (fnk [x13 x14])
                    :x18 (fnk [x14 x16])
                    :x19 (fnk [x16])
                    :x20 (fnk [x12 x16 x02])
                    :x21 (fnk [x01 x18])
                    :x22 (fnk [x19])
                    :x23 (fnk [x03 x06 x21 x22])
                    })


#_(def example-graph {:B (fnk [A])
                    :C (fnk [B])
                    :D (fnk [A B Y])
                    :E (fnk [A C])
                    :F (fnk [B Z])
                    :G (fnk [E F])
                    :H (fnk [E])
                    :Y (fnk [X])
                    :Z (fnk [Y])})

#_(def example-graph layout/layout-graph)


(defn get-graph-nodes [g]
  (into #{} (concat (mapcat pfnk/input-schema-keys (vals g)) (keys g))))

(defn get-graph-edges [g nodes]
  (->> nodes
       (map (juxt identity
                  (fn [node] (some-> (get g node) (pfnk/input-schema-keys)))))
       (filter (comp some? second))
       (mapcat (fn [[dest srcs]]
                 (map (fn [src]
                        [(name src) (name dest)])
                      srcs)))))

(def selected-node-id (atom nil))


#_(add-watch selected-node-id :selected-node-focus
           (fn [_ _ prev-selected new-selected-id]
             (let [{:keys [dummy-graph xy-coordinates node-to-layer]} @layout-result
                   selected-layer (get node-to-layer new-selected-id)
                   succs-selected (graph/successors dummy-graph new-selected-id)]
               (doseq [succ succs-selected]
                 (when (not= (dec selected-layer) (get node-to-layer succ))
                   (let [dummy-node {:src new-selected-id :dest succ :layer (dec selected-layer)}
                         coords (get xy-coordinates dummy-node)]
                     (println "new-coords" coords)
                     ))))
             (println "prev" prev-selected "new" new-selected-id)
             #_(let [graph-desc-state @graph-desc-atom
                   nodes-description (get graph-desc-state "nodes")
                   sizes (get graph-desc-state "sizes")]
               (remove-highlight-arrows)
               (when prev-selected
                 (revert-to-default-positions nodes-description prev-selected))
               (if new-selected-id
                 (highlight-selection nodes-description sizes new-selected-id)
                 (revert-to-normal-display)))))

(defn add-click-listener [ele callback]
  (gevents/listen
    ele
    goog.events.EventType.CLICK
    callback))

(defn start []
  #_(add-click-listener
    (.-body js/document)
    (fn [_]
      (reset! selected-node-id nil)))
  #_(doseq [node-ele (seq (query ".graph-node"))]
    (add-click-listener node-ele
                        (fn [evt]
                          (reset! selected-node-id (.-id node-ele))
                          (.preventDefault evt)
                          (.stopPropagation evt)))))



(defn init []
  (let [size-atom (atom {})
        edges (get-graph-edges example-graph (get-graph-nodes example-graph))]
    (rum/mount (layout size-atom
                       (concat
                         (map (fn [[node-name keywordfn]]
                                {:node/view (code-node size-atom (name node-name) keywordfn)
                                 :node/id   (name node-name)})
                              example-graph)
                         (map (fn [node-name]

                                {:node/view (input-node size-atom (name node-name))
                                 :node/id   (name node-name)})
                              (set/difference (into #{}
                                                    (mapcat pfnk/input-schema-keys)
                                                    (vals example-graph))
                                              (into #{}
                                                    (keys example-graph)))))
                       edges)
               (. js/document (getElementById "container")))
    (start)))
