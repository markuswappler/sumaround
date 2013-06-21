(ns euwe.core-test
  (:use clojure.test
        euwe.core))

(defdraw draw-test-1 [players games]
  [[(players 0) (players 1)]
   [(players 2) (players 3)]])

(defdraw draw-test-2 [_ games]
  [(reduce + games) 42 4711])

(defdraw draw-test-3 
  "This is a docstring."
  [players games]
  nil)

(deftest test-defdraw
  (testing "draw-test-1"
           (let [players ["Anton" "Berta" "Caesar" "Dora"]
                 new-games [["Anton" "Berta"] ["Caesar" "Dora"]]]
             (is (= {:players players
                     :games new-games}
                    (draw-test-1 {:players players})))
             (is (= {:players players
                     :games new-games}
                    (draw-test-1 {:players players
                                  :games nil})))
             (is (= {:players players
                     :games new-games}
                    (draw-test-1 {:players players
                                  :games []})))
             (is (= {:players players
                     :games (concat [["Anton" "Caesar"]] new-games)}
                    (draw-test-1 {:players players
                                  :games [["Anton" "Caesar"]]})))))
  (testing "draw-test-2"
           (let [players ["A" "B" "C" "D"]]
             (is (= {:players players
                     :games [1 2 3 4 10 42 4711]}
                    (draw-test-2 {:players players
                                  :games [1 2 3 4]})))))
  (testing "draw-test-3"
           (let [data {:players ["A" "B" "C" "D"]
                       :games [["A" "B"] ["C" "D"]]}]
             (is (= data (draw-test-3 data))))))

(deftest test-round-robin
  (let [rr (fn [n]
             (->> {:players (range 1 (inc n))}
               round-robin
               :games
               (map (fn [{:keys [player-1 player-2]}] 
                      [player-1 player-2]))
               set))]
    (is (= #{} 
           (rr 1)))
    (is (= #{[1 2]} 
           (rr 2)))
    (is (= #{[2 3] 
             [1 2] 
             [3 1]} 
           (rr 3)))
    (is (= #{[1 4] [2 3] 
             [4 3] [1 2] 
             [2 4] [3 1]} 
           (rr 4)))
    (is (= #{[2 5] [3 4] 
             [5 3] [1 2]
             [3 1] [4 5]
             [1 4] [2 3]
             [4 2] [5 1]}
           (rr 5)))
    (is (= #{[1 6] [2 5] [3 4]
             [6 4] [5 3] [1 2]
             [2 6] [3 1] [4 5]
             [6 5] [1 4] [2 3]
             [3 6] [4 2] [5 1]}
           (rr 6)))
    (is (= #{[2 7] [3 6] [4 5]
             [6 4] [7 3] [1 2]
             [3 1] [4 7] [5 6]
             [7 5] [1 4] [2 3]
             [4 2] [5 1] [6 7]
             [1 6] [2 5] [3 4]
             [5 3] [6 2] [7 1]}
           (rr 7)))
    (is (= #{[1 8] [2 7] [3 6] [4 5]
             [8 5] [6 4] [7 3] [1 2]
             [2 8] [3 1] [4 7] [5 6]
             [8 6] [7 5] [1 4] [2 3]
             [3 8] [4 2] [5 1] [6 7]
             [8 7] [1 6] [2 5] [3 4]
             [4 8] [5 3] [6 2] [7 1]}
           (rr 8)))))

(deftest test-second-leg
  (let [games-1 [(make-game 1 2)]
        games-2 [(make-game 2 3)
                 (make-game 1 4)]
        games-3 [(make-game 1 2)
                 (make-game 1 3)
                 (make-game 1 4)]]
    (is (= {:games []} (second-leg {})))
    (is (= {:games []} (second-leg {:games nil})))
    (is (= {:games []} (second-leg {:games []})))
    (is (= {:games (concat games-1 
                           [(make-game 2 1)])}
           (second-leg {:games games-1})))
    (is (= {:games (concat games-2
                           [(make-game 3 2)
                            (make-game 4 1)])}
           (second-leg {:games games-2})))
    (is (= {:games (concat games-3
                           [(make-game 2 1)
                            (make-game 3 1)
                            (make-game 4 1)])}
           (second-leg {:games games-3})))))