(ns euwe.italia90
  (:use clojure.test 
        euwe.score))

;; data from 1990 FIFA world cup, group D

(def teams ["ger" "yug" "col" "uae"])
  
(def games [{:home "uae" :away "col" :home-goals 0 :away-goals 2}            
            {:home "ger" :away "yug" :home-goals 4 :away-goals 1}
            {:home "yug" :away "col" :home-goals 1 :away-goals 0}
            {:home "ger" :away "uae" :home-goals 5 :away-goals 1}
            {:home "ger" :away "col" :home-goals 1 :away-goals 1}
            {:home "yug" :away "uae" :home-goals 4 :away-goals 1}])

(def scored (choice
              (player? :home) (game-score :home-goals)
              (player? :away) (game-score :away-goals)))

(def against (choice
               (player? :home) (game-score :away-goals)
               (player? :away) (game-score :home-goals)))

(def wins (sum-up (choice (gt? scored against) one)))
(def draws (sum-up (choice (eq? scored against) one)))
(def losses (sum-up (choice (lt? scored against) one)))

(def cnt (add (add wins draws) losses))
(def points-scored (add (mult two wins) draws))
(def points-against (add (mult two losses) draws))

(def goals-scored (sum-up scored))
(def goals-against (sum-up against))
(def goals-diff (sub goals-scored goals-against))

(defn make-table [teams games]
  (for [t teams]
    {:name t
     :cnt (cnt t games)
     :wins (wins t games)
     :draws (draws t games)
     :losses (losses t games)
     :points-scored (points-scored t games)
     :points-against (points-against t games)
     :goals-scored (goals-scored t games)
     :goals-against (goals-against t games)
     :goals-diff (goals-diff t games)}))

(defn exp-row [team cnt wins draws losses points-scored points-against 
               goals-scored goals-against goals-diff] 
  {:name team
   :cnt cnt
   :wins wins
   :draws draws
   :losses losses
   :points-scored points-scored
   :points-against points-against
   :goals-scored goals-scored
   :goals-against goals-against
   :goals-diff goals-diff})

(defn row [team table]
  (first (filter #(= team (:name %)) table))) 

(deftest test-table
  (let [table (make-table teams games)]
    (is (= (exp-row "ger" 3 2 1 0 5 1 10 3 7) (row "ger" table)))
    (is (= (exp-row "yug" 3 2 0 1 4 2 6 5 1) (row "yug" table)))
    (is (= (exp-row "col" 3 1 1 1 3 3 3 2 1) (row "col" table)))
    (is (= (exp-row "uae" 3 0 0 3 0 6 2 11 -9) (row "uae" table)))))  