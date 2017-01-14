(ns vizgraph.layout-test
  (:require [clojure.test :refer :all]
            [vizgraph.layout :refer :all]
            [loom.graph :as graph]
            plumbing.graph))

(def sample-graph1
  (graph/digraph ["a" "aa"] ["b" "bb"] ["c" "cc"]))

(def long-edge-graph
  (graph/digraph ["a" "ab"]
                 ["b" "ab"]
                 ["ab" "aab"]
                 ["b" {:src "b", :dest "aab", :dummy-index 1, :layer 1}]
                 [{:src "b", :dest "aab", :dummy-index 1, :layer 1} "aab"]))

#_(def bigger-graph1
  (graph/digraph ["n" "nnx"]
                 ["nnx" "xxxx"]
                 ["m1" "mmm"]
                 ["m2" "mm"]
                 ["mm" "mmm"]
                 ["mmm" "xxxx"]))

(deftest test-crossing-count
  (is (= 0 (crossing-count sample-graph1
                           {1 ["a" "b" "c"]
                            0 ["aa" "bb" "cc"]}
                           1)))
  (is (= 1 (crossing-count sample-graph1
                           {1 ["b" "a" "c"]
                            0 ["aa" "bb" "cc"]}
                           1)))
  (is (= 0 (crossing-count-involving-layer long-edge-graph 3 {2 ["a" "b"],
                                                              1 ["ab" {:src "b", :dest "aab", :dummy-index 1, :layer 1}],
                                                              0 ["aab"]} 2)))
  (is (= 1 (crossing-count-involving-layer long-edge-graph 3 {2 ["b" "a"],
                                                              1 ["ab" {:src "b", :dest "aab", :dummy-index 1, :layer 1}],
                                                              0 ["aab"]} 2)))
  (is (= 1 (crossing-count-involving-layer long-edge-graph 3 {2 ["b" "a"],
                                                              1 ["ab" {:src "b", :dest "aab", :dummy-index 1, :layer 1}],
                                                              0 ["aab"]} 1)))
  (is (= 0 (crossing-count-involving-layer long-edge-graph
                                           3
                                           {2 ["b" "a"],
                                            1 ["ab" {:src "b", :dest "aab", :dummy-index 1, :layer 1}],
                                            0 ["aab"]}
                                           0)))
  (is (= 1 (crossing-count-involving-layer sample-graph1 2 {1 ["b" "a" "c"]
                                                            0 ["aa" "bb" "cc"]} 1)))
  (is (= 1 (crossing-count-involving-layer sample-graph1 2 {1 ["b" "a" "c"]
                                                            0 ["aa" "bb" "cc"]} 0)))
  (is (= 2 (crossing-count sample-graph1
                           {1 ["b" "c" "a"]
                            0 ["aa" "bb" "cc"]}
                           1)))
  (is (= 3 (crossing-count sample-graph1
                           {1 ["c" "b" "a"]
                            0 ["aa" "bb" "cc"]}
                           1))))

(deftest test-weighted-median-sorting
  (is (= {1 ["yb" "za" "xc"]
          0 ["bb" "aa" "cc"]}
         (apply weighted-median-sorted
                {1 ["yb" "za" "xc"]
                 0 ["aa" "bb" "cc"]}
                (alternating-directions (graph/digraph ["za" "aa"] ["yb" "bb"] ["xc" "cc"]) 2 0))))
  (is (= {1 ["a" "b" "c"]
          0 ["aa" "bb" "cc"]}
         (apply weighted-median-sorted
                {1 ["b" "c" "a"]
                 0 ["aa" "bb" "cc"]}
                (alternating-directions sample-graph1 2 1)))))

(deftest test-transpose
  (is (= {1 ["b" "a"]
          0 ["bb" "aa"]}
         (transpose (graph/digraph ["a" "aa"] ["b" "bb"])
                    2
                    {1 ["b" "a"]
                     0 ["aa" "bb"]}))))

(deftest test-crossing-minimization
  (let [input {:dummy-graph sample-graph1
               :all-node-to-layer {"a" 1
                                   "aa" 0
                                   "b" 1
                                   "bb" 0
                                   "c" 1
                                   "cc" 0}
               :height      2}
        result (plumbing.graph/run crossing-minimization-graph input)]
    (is (= {1 ["a" "b" "c"]
            0 ["aa" "bb" "cc"]}
           (:layer-to-nodes result)))))