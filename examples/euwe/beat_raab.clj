(ns euwe.beat-raab
  (:use clojure.test 
        euwe.score))

; Schlag den Raab
; episode 38, 15.12.2012
; episode 41, 11.05.2013

(def players [:raab :candidate])

(def games38 [{:no 1 :winner :candidate}
              {:no 2 :winner :raab}
              {:no 3 :winner :candidate}
              {:no 4 :winner :raab}
              {:no 5 :winner :raab}
              {:no 6 :winner :raab}
              {:no 7 :winner :candidate}
              {:no 8 :winner :candidate}
              {:no 9 :winner :raab}
              {:no 10 :winner :raab}
              {:no 11 :winner :candidate}
              {:no 12 :winner :candidate}
              {:no 13 :winner :candidate}
              {:no 14 :winner :raab}
              {:no 15 :winner :candidate}])

(def games41 [{:no 1 :winner :candidate}
              {:no 2 :winner :raab}
              {:no 3 :winner :raab}
              {:no 4 :winner :candidate}
              {:no 5 :winner :candidate}
              {:no 6 :winner :raab}
              {:no 7 :winner :raab}
              {:no 8 :winner :candidate}
              {:no 9 :winner :raab}
              {:no 10 :winner :candidate}
              {:no 11 :winner :raab}
              {:no 12 :winner :candidate}
              {:no 13 :winner :raab}
              {:no 14 :winner :raab}
              {:no 15}])

(deftable result
  :score [points (sum 
                   (choice 
                     (player? :winner) 
                     (game-score :no)))]
  :sort [points >]
  :player player
  :yield [player points])
  
(deftest test-result
  (let [[winner38 looser38] (result players games38)
        [winner41 looser41] (result players games41)]
    (is (= [:candidate 70] winner38))
    (is (= [:raab 50] looser38))
    (is (= [:raab 65] winner41))
    (is (= [:candidate 40] looser41))))