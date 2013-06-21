(ns euwe.hastings3031
  (:use clojure.test 
        euwe.score))

;; 11th Hastings Christmas Chess Festival 1930/31

(def players ["Euwe" "Capablanca" "Sultan Khan" "Michell" "Yates"
              "Thomas" "Winter" "Tylor" "Menchik" "Colle"])

(def games [{:white "Capablanca" :black "Colle" :result :1}
            {:white "Menchik" :black "Yates" :result :=}
            {:white "Sultan Khan" :black "Michell" :result :1}
            {:white "Thomas" :black "Tylor" :result :1}
            {:white "Winter" :black "Euwe" :result :0}
            
            {:white "Capablanca" :black "Yates" :result :1}
            {:white "Colle" :black "Sultan Khan" :result :0}
            {:white "Euwe" :black "Menchik" :result :0}
            {:white "Michell" :black "Thomas" :result :1}
            {:white "Tylor" :black "Winter" :result :=}
            
            {:white "Menchik" :black "Tylor" :result :=}
            {:white "Sultan Khan" :black "Capablanca" :result :1}
            {:white "Thomas" :black "Colle" :result :1}
            {:white "Winter" :black "Michell" :result :=}
            {:white "Yates" :black "Euwe" :result :0}
            
            {:white "Colle" :black "Winter" :result :=}
            {:white "Euwe" :black "Capablanca" :result :=}
            {:white "Michell" :black "Menchik" :result :1}
            {:white "Sultan Khan" :black "Thomas" :result :1}
            {:white "Tylor" :black "Yates" :result :0}
            
            {:white "Capablanca" :black "Thomas" :result :=}
            {:white "Euwe" :black "Tylor" :result :1}
            {:white "Menchik" :black "Colle" :result :0}
            {:white "Winter" :black "Sultan Khan" :result :1}
            {:white "Yates" :black "Michell" :result :0}
            
            {:white "Capablanca" :black "Tylor" :result :1}
            {:white "Colle" :black "Yates" :result :=}
            {:white "Michell" :black "Euwe" :result :0}
            {:white "Sultan Khan" :black "Menchik" :result :1}
            {:white "Thomas" :black "Winter" :result :1}
            
            {:white "Euwe" :black "Colle" :result :1}
            {:white "Menchik" :black "Thomas" :result :1}
            {:white "Tylor" :black "Michell" :result :1}
            {:white "Winter" :black "Capablanca" :result :0}
            {:white "Yates" :black "Sultan Khan" :result :=}
            
            {:white "Colle" :black "Tylor" :result :=}
            {:white "Michell" :black "Capablanca" :result :=}
            {:white "Sultan Khan" :black "Euwe" :result :0}
            {:white "Thomas" :black "Yates" :result :0}
            {:white "Winter" :black "Menchik" :result :1}
            
            {:white "Euwe" :black "Thomas" :result :=}
            {:white "Menchik" :black "Capablanca" :result :0}
            {:white "Michell" :black "Colle" :result :1}
            {:white "Tylor" :black "Sultan Khan" :result :=}
            {:white "Yates" :black "Winter" :result :1}])

(def points (sum-up
              (choice
                (player? :white)
                (choice (eq? (game-score :result) (always :1)) one
                        (eq? (game-score :result) (always :=)) half
                        (eq? (game-score :result) (always :0)) zero)
                (player? :black)
                (choice (eq? (game-score :result) (always :1)) zero
                        (eq? (game-score :result) (always :=)) half
                        (eq? (game-score :result) (always :0)) one))))

(defn make-table [players games]
  (for [p players]
    [p (double (points p games))]))

(deftest test-table
  (is (= [["Euwe" 7.0] 
          ["Capablanca" 6.5]
          ["Sultan Khan" 6.0]
          ["Michell" 5.0]
          ["Yates" 4.5]
          ["Thomas" 4.0]
          ["Winter" 3.5]
          ["Tylor" 3.0]
          ["Menchik" 3.0]
          ["Colle" 2.5]]
         (make-table players games))))