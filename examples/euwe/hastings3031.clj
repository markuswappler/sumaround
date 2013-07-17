(ns euwe.hastings3031  
  (:use clojure.test 
        euwe.score)
  (:require [clojure.math.numeric-tower :as num]))

;; 11th Hastings Christmas Chess Festival 1930/31
;; points = 1 for win, 0.5 for draw; 0 for loss
;; soberg (sonneborn-berger) = total points of opponent for win,
;;                             half of the total points of opponent for draw,
;;                             0 for loss                           

(def players ["Capablanca"
              "Colle"
              "Euwe"  
              "Menchik" 
              "Michell" 
              "Sultan Khan"
              "Thomas"               
              "Tylor"              
              "Winter"
              "Yates"])

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

(def white-win? (choice (eq? (game-score :result) (always :1)) yes))
(def draw? (choice (eq? (game-score :result) (always :=)) yes))
(def black-win? (choice (eq? (game-score :result) (always :0)) yes))
(def game-points (choice
                   (player? :white) (choice white-win? one
                                            draw? half
                                            black-win? zero)
                   (player? :black) (choice white-win? zero
                                            draw? half
                                            black-win? one)))
(def opponent (choice
                (player? :white) (game-score :black)
                (player? :black) (game-score :white)))

(deftable points-table
  :player player
  :score [points (sum game-points)]
  :yield {:name player 
          :points (double points)})

(deftable table
  :depend points-table
  :player player
  :score [points (lookup points-table :name :points)
          soberg (sum (mult 
                        game-points 
                        (player-> opponent points)))]
  :yield {:name player 
          :points points 
          :soberg soberg}
  :sort [points >
         soberg >])

(deftable alphabetic
  :sort [player compare]
  :depend [table points-table]
  :player player
  :score [points (lookup points-table :name :points)
          soberg (lookup table :name :soberg)]
  :yield [points soberg])  

(deftable statistics
  :score [white-wins (sum (choice white-win? one))
          draws (sum (choice draw? one))
          black-wins (sum (choice black-win? one))
          total (sum one)]
  :yield (let [round (fn [x] (/ (num/round (* 10 x)) 10))]
           {:total total
            :1 white-wins 
            := draws 
            :0 black-wins 
            :% (double (round (* 100 
                                 (/ (+ white-wins (* 0.5 draws)) 
                                    total))))}))

(deftest test-table
  (is (= ["Euwe" 7.0 29.75
          "Capablanca" 6.5 24.5
          "Sultan Khan" 6.0 24.75
          "Michell" 5.0 19.0
          "Yates" 4.5 16.25
          "Thomas" 4.0 15.75
          "Winter" 3.5 14.25
          "Menchik" 3.0 14.75
          "Tylor" 3.0 12.5          
          "Colle" 2.5 8.5]
         (mapcat 
           (fn [row] [(row :name) (row :points) (row :soberg)])
           (table players games)))))

(deftest test-alphabetic
  (is (= [6.5 24.5   ; Capablanca
          2.5 8.5    ; Colle
          7.0 29.75  ; Euwe
          3.0 14.75  ; Menchik
          5.0 19.0   ; Michell
          6.0 24.75  ; Sultan Khan
          4.0 15.75  ; Thomas
          3.0 12.5   ; Tylor
          3.5 14.25  ; Winter
          4.5 16.25] ; Yates
         (apply concat (alphabetic players games)))))

(deftest test-statistics
  (is (= {:total 45
          :1 20 
          := 13 
          :0 12
          :% 58.9}
         (statistics games))))