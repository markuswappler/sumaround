(ns euwe.score-test
  (:use clojure.test
        euwe.score))

(deftest test-map-comparer
  (is (zero? ((map-comparer) [] [])))
  (is (zero? ((map-comparer) [] [1])))
  (is (zero? ((map-comparer) [1 2] [])))
  (is (zero? ((map-comparer) [1] [2])))
  
  (is (neg? ((map-comparer <) [1] [2])))
  (is (zero? ((map-comparer <) [2] [2])))
  (is (pos? ((map-comparer <) [3] [2])))
           
  (is (pos? ((map-comparer >) [1] [2])))
  (is (zero? ((map-comparer >) [2] [2])))
  (is (neg? ((map-comparer >) [3] [2])))
  
  (is (neg? ((map-comparer compare) ["alpha"] ["beta"])))
  (is (zero? ((map-comparer compare) ["beta"] ["beta"])))
  (is (pos? ((map-comparer compare) ["gamma"] ["beta"])))
  
  (is (neg? ((map-comparer <) [1 5] [2])))
  (is (zero? ((map-comparer <) [2 5] [2 0])))
  (is (pos? ((map-comparer <) [3 5] [2 7 9])))
  
  (is (neg? ((map-comparer < <) [1 2] [2 3])))
  (is (pos? ((map-comparer < <) [3 2] [2 3])))
  (is (neg? ((map-comparer < <) [2 2] [2 3])))
  (is (pos? ((map-comparer < <) [2 2] [2 1]))) 
  (is (zero? ((map-comparer < <) [2 3] [2 3])))
  
  (is (neg? ((map-comparer < >) [1 2] [2 3])))
  (is (pos? ((map-comparer < >) [3 2] [2 3])))
  (is (pos? ((map-comparer < >) [2 2] [2 3])))
  (is (neg? ((map-comparer < >) [2 2] [2 1])))  
  (is (zero? ((map-comparer < >) [2 3] [2 3])))
  
  (is (zero? ((map-comparer > < <) [1 2 3 4] [1 2 3 0])))
  (is (neg? ((map-comparer > < <) [1 2 3] [1 2 4])))
  (is (pos? ((map-comparer > < <) [1 2 3] [1 3 3])))
  (is (neg? ((map-comparer > < <) [1 2 3] [0 2 3]))))