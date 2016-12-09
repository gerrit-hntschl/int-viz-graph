(ns vizgraph.app
  (:require [rum.core :as rum]
            [cljs.pprint :as pprint]
            [clojure.string :as str]
            [loom.graph :as graph]
            [cljs.reader :as reader]
            [schema.core :as schema]
            [vizgraph.layout :as layout]
            [plumbing.core :refer [map-vals]]
            [plumbing.fnk.pfnk :as pfnk]
            [clojure.set :as set])
  (:require-macros [vizgraph.core :refer [fnk]]))

(enable-console-print!)

(def sized {:did-mount (fn [state]
                         (println "sized did-mount")
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
        formatted-code (-> (reader/read-string source)
                           (pprint/write
                             :dispatch pprint/code-dispatch)
                           (with-out-str)
                           (.replace \- \u2011))
        code-with-marked-input (reduce (fn [code input-name]
                                         (-> code
                                             (str/replace input-name (str "@@@" input-name "@@@"))))
                                       formatted-code
                                       input-names)
        code-markup (-> code-with-marked-input
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


(rum/defcs layout <
  {:did-mount (fn [state]
                (println "layout did-mount")
                (let [[size-atom nodes edges] (:rum/args state)
                      {:keys [xy-coordinates edge->node-coordinates size]} (layout/hierarchical @size-atom edges)]
                  (rum/request-render (:rum/react-component state))
                  (assoc state ::id->coordinates xy-coordinates
                               ::all-edges edge->node-coordinates
                               ::graph-size size)))}
  [state size-atom nodes edges]
  (let [child-coordinates (or (::id->coordinates state)
                              (map #(update % 0 str) (iterate #(update % 0 inc) [0 0])))]
    [:div {:style {:position "relative"}}
     [:svg (or (map-vals (partial + 150) (::graph-size state)) {})
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
             (let [node-coordinates (get (::all-edges state) edge)]
               (when (seq node-coordinates)
                 (condp = (count node-coordinates)
                   2 [:polyline {:key        (str edge)
                                 :id         (str edge)
                                 :marker-end "url(#arrow)"
                                 :fill       "none"
                                 :stroke     "black"
                                 :points     (str/join " " (map (fn [{:keys [x y]}] (str x "," y))
                                                                node-coordinates))
                                 :style      {:stroke       "#000000"
                                              :stroke-width 1}}]
                   3 [:path {:d          (str "M" (str/join "," ((juxt :x :y) (first node-coordinates)))
                                              " Q" (str/join " " (map (fn [{:keys [x y]}] (str x "," y))
                                                                      (rest node-coordinates))))
                             :fill       "transparent"
                             :stroke     "black"
                             :marker-end "url(#arrow)"
                             :key        (str edge)
                             :id         (str edge)}]
                   [:path {:d          (str "M" (str/join "," ((juxt :x :y) (first node-coordinates)))
                                            " C" (str/join ", " ((juxt :x :y) (second node-coordinates)))
                                            " " (str/join " " (map (fn [{:keys [x y]}] (str x "," y))
                                                                   (take-last 2 node-coordinates))))
                           :fill       "transparent"
                           :stroke     "black"
                           :marker-end "url(#arrow)"
                           :key        (str edge)
                           :id         (str edge)}]
                   ))))
           edges)]
     [:div
      (map (fn [{:node/keys [view id]}]
             (println "id" id "raw x" (get-in child-coordinates [id :x]))
             [:div {:style {:top      (get-in child-coordinates [id :y])
                            :position "absolute"
                            :left     (get-in child-coordinates [id :x])}
                    :key   id}
              view])
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

#_(def missing-users-graph
  ;; input -> import-usernames-all & dbval & jira
  {:db-usernames-all                 (fnk [dbval]
                                       (all-usernames dbval))
   :import-usernames-new           (fnk [import-usernames-all db-usernames-all]
                                     (set/difference import-usernames-all db-usernames-all))
   :jira-users-new                   (fnk [jira import-usernames-new]
                                       (when (not-empty import-usernames-new)
                                         (users jira import-usernames-new)))
   :domain-users-new                 (fnk [jira-users-new]
                                       (mapv to-db-user
                                             jira-users-new))})

#_(def example-graph
  ;; input:
  ;; dbval :- Datomic database value,
  ;; today :- Date of today with time 1 millisecond before tomorrow
  ;; jira :- Jira Client implementation
  (merge
    {:re-import?           (fnk [dbval]
                             (boolean (any-work-date dbval)))
     :current-period-start (fnk [today]
                             (domain/current-period-start today))
     :import-start-date    (fnk [re-import? today current-period-start]
                             (if re-import?
                               current-period-start
                               (time/first-day-of-the-month (time/year today) 1)))
     :import-end-date      (fnk [today]
                             (time/local-date (time/year today) 12 31))
     :worklogs-retrieved   (fnk [jira import-start-date import-end-date]
                             ;; returns a map containing all intermediate state used to retrieve actual worklogs
                             ;; other entries are not used, but useful for debugging
                             (worklogs jira import-start-date import-end-date))
     :worklogs-all         (fnk worklogs-all
                             [worklogs-retrieved]
                             ;; simplify access to actual worklogs
                             (:worklogs worklogs-retrieved))
     :import-usernames-all (fnk [worklogs-all]
                             (into #{} (map :username) worklogs-all))}
    missing-users-graph
    {:jira-usernames-new               (fnk [jira-users-new]
                                         (into #{} (map :name) jira-users-new))
     :worklogs-usernames-unknown       (fnk [jira-usernames-new import-usernames-new]
                                         (set/difference import-usernames-new jira-usernames-new))
     :issue-ids-all                    (fnk [worklogs-all]
                                         (into #{} (map :issue_id) worklogs-all))
     :db-ticket-ids-all                (fnk [dbval]
                                         (all-domain-ticket-ids dbval))
     :issue-ids-new                    (fnk [issue-ids-all db-ticket-ids-all]
                                         (set/difference issue-ids-all db-ticket-ids-all))
     :issues-new-parsed-json           (fnk [jira issue-ids-new]
                                         (issues jira issue-ids-new))
     :issues-new-coerced               (fnk [issues-new-parsed-json]
                                         (mapv model/to-jira-issue
                                               issues-new-parsed-json))
     :tickets-new                      (fnk [issues-new-coerced]
                                         (mapv jira-issue-to-datomic-ticket issues-new-coerced))
     :db-customer-ids-all              (fnk [dbval]
                                         (all-domain-customer-ids dbval))
     ;; can the component of a jira issue change? here we assume that only new issues can introduce new components
     :jira-customers-new               (fnk [issues-new-coerced]
                                         (distinct (mapv extract-customer-from-jira-issue issues-new-coerced)))
     :jira-customer-ids                (fnk [jira-customers-new]
                                         (into #{} (map :id) jira-customers-new))
     :customer-ids-new                 (fnk [db-customer-ids-all jira-customer-ids]
                                         (set/difference jira-customer-ids db-customer-ids-all))
     :domain-customers-new             (fnk [customer-ids-new jira-customers-new]
                                         (mapv jira-component-to-datomic-customer
                                               (filter (comp customer-ids-new :id) jira-customers-new)))
     :worklogs-known-usernames         (fnk [worklogs-all worklogs-usernames-unknown]
                                         (remove (comp worklogs-usernames-unknown :username) worklogs-all))
     :db-worklog-ids-in-import-range   (fnk [re-import? dbval import-start-date import-end-date]
                                         (when re-import?
                                           (all-domain-worklog-ids-in-range dbval
                                                                            (time-coerce/to-date import-start-date)
                                                                            (time-coerce/to-date import-end-date))))
     :jira-worklog-ids-in-import-range (fnk [worklogs-known-usernames]
                                         (into #{} (map :worklog_id) worklogs-known-usernames))
     :jira-worklog-ids-deleted         (fnk [re-import? db-worklog-ids-in-import-range jira-worklog-ids-in-import-range]
                                         (if re-import?
                                           (set/difference db-worklog-ids-in-import-range jira-worklog-ids-in-import-range)
                                           #{}))
     :domain-worklogs-new              (fnk [worklogs-known-usernames]
                                         (mapv jira-worklog-to-datomic-worklog worklogs-known-usernames))
     :domain-worklogs-deleted          (fnk [jira-worklog-ids-deleted]
                                         (mapv worklog-retraction jira-worklog-ids-deleted))
     :domain-worklogs-transaction      (fnk [domain-worklogs-new domain-worklogs-deleted]
                                         (concat domain-worklogs-new domain-worklogs-deleted))
     :db-transactions                  (fnk [domain-users-new domain-customers-new tickets-new domain-worklogs-transaction]
                                         (remove empty?
                                                 (vector domain-users-new
                                                         domain-customers-new
                                                         tickets-new
                                                         domain-worklogs-transaction)))
     :db-transactions-stats            (fnk [domain-users-new domain-customers-new tickets-new domain-worklogs-deleted domain-worklogs-new]
                                         (zipmap [:new-users :new-customers :new-tickets :deleted-worklogs :worklogs-in-period]
                                                 (map count
                                                      [domain-users-new domain-customers-new tickets-new domain-worklogs-deleted domain-worklogs-new])))}))

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


(def example-graph {:a03 (fnk [x01 x02])
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

(defn init []
  (let [size-atom (atom {})]
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
                       (get-graph-edges example-graph (get-graph-nodes example-graph)))
               (. js/document (getElementById "container")))))
