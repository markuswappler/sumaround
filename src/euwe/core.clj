(ns euwe.core)

(defn- rr 
  "Computes the (zero-based) k-th round of a 
  round-robin-tournament with n players.
  Returns a list of vectors [p1 p2], 0<=p1,p2<=n-1, 
  which means p1 vs. p2. The order is relevant
  (e.g. colors in chess) and balanced for 
  each player."
  [n k]
  (if (even? n)
    (let [n-1 (dec n)
          opponent (fn [p]
                     (let [opp (mod (- k p) n-1)]
                       (if (= p opp)
                         n-1
                         opp)))
          correct-order? (fn [p1 p2]
                           (or
                             (and (= n-1 p2) 
                                  (> (quot n 2) p1))
                             (and (not= n-1 p2) 
                                  (odd? (+ p1 p2)))))]
      (->> (range n-1)
        (map (fn [p] [p (opponent p)]))
        (filter (fn [[p1 p2]] (< p1 p2)))
        (map (fn [[p1 p2]] (if (correct-order? p1 p2) 
                             [p1 p2] 
                             [p2 p1])))))
    (->> (rr (inc n) k)
      (filter (fn [[p1 p2]] (and (not= n p1) (not= n p2)))))))
      
(defn round-robin 
  "Takes a sequence of players and computes
  a list of all games of a round-robin-tournament
  with these players. Each game is represented
  by a vector [p1 p2], which means p1 vs. p2.
  The order is relevant (e.g. colors in chess) 
  and balanced for each player."
  [players]
  (let [players-cnt (count players)
        rounds-cnt (if (even? players-cnt) 
                     (dec players-cnt) 
                     players-cnt)]
    (->> (range rounds-cnt)
      (mapcat (partial rr players-cnt))
      (map (fn [[p1 p2]] [(nth players p1) (nth players p2)])))))