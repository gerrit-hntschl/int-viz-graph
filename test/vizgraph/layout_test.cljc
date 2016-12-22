(ns vizgraph.layout-test
  (:require [clojure.test :refer :all]
            [vizgraph.layout :refer :all]
            [loom.graph :as graph]))

(def sample-graph1
  (graph/digraph ["a" "aa"] ["b" "bb"] ["c" "cc"]))

(deftest test-crossing-count
  (is (= 0 (crossing-count sample-graph1
                           {1 ["a" "b" "c"]
                            0 ["aa" "bb" "cc"]}
                           1)))
  (is (= 1 (crossing-count sample-graph1
                           {1 ["b" "a" "c"]
                            0 ["aa" "bb" "cc"]}
                           1)))
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
  (is (= {1 ["a" "b" "c"]
          0 ["aa" "bb" "cc"]}
         (apply weighted-median-sorted
                {1 ["b" "a" "c"]
                 0 ["aa" "bb" "cc"]}
                (alternating-directions sample-graph1 2 0))))
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

#_(deftest test-crossing-minimization
         (let [input {:dummy-graph sample-graph1
                      :all-node-to-layer {"a" 1
                                          "aa" 0
                                          "b" 1
                                          "bb" 0
                                          "c" 1
                                          "cc" 0}
                      :height      2}]))