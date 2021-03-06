(ns euwe.italia90
  (:use clojure.test 
        euwe.score))

;; data from 1990 FIFA world cup, group D

(def teams ["col" "ger" "uae" "yug"])
  
(def games [{:home "uae" :away "col" :home-goals 0 :away-goals 2}            
            {:home "ger" :away "yug" :home-goals 4 :away-goals 1}
            {:home "yug" :away "col" :home-goals 1 :away-goals 0}
            {:home "ger" :away "uae" :home-goals 5 :away-goals 1}
            {:home "ger" :away "col" :home-goals 1 :away-goals 1}
            {:home "yug" :away "uae" :home-goals 4 :away-goals 1}])

(deftable table
  :player team
  :score [scored (choice
                   (player? :home) (game-score :home-goals)
                   (player? :away) (game-score :away-goals))          
          against (choice
                    (player? :home) (game-score :away-goals)
                    (player? :away) (game-score :home-goals))          
          wins (sum (choice (gt? scored against) one))
          draws (sum (choice (eq? scored against) one))
          losses (sum (choice (lt? scored against) one))          
          cnt (plus (plus wins draws) losses)          
          points-scored (plus (mult two wins) draws)
          points-against (plus (mult two losses) draws)   
          goals-scored (sum scored)
          goals-against (sum against)   
          goals-diff (minus goals-scored goals-against)]
  :yield [team cnt wins draws losses points-scored points-against 
          goals-scored goals-against goals-diff]
  :sort [points-scored >
         points-against <
         goals-diff >
         goals-scored >])

(deftest test-table
  (is (= [["ger" 3 2 1 0 5 1 10 3 7]
          ["yug" 3 2 0 1 4 2 6 5 1]
          ["col" 3 1 1 1 3 3 3 2 1]
          ["uae" 3 0 0 3 0 6 2 11 -9]]
         (table teams games))))