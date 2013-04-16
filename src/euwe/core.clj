(ns euwe.core)

(defmacro defdraw
  "fixme: create docstring"
  [name & decl]
  (let [[name-doc args body] 
        (if (string? (first decl))
          [[name (first decl)] (second decl) (nnext decl)]
          [[name] (first decl) (next decl)])
        [players-arg games-arg] args
        players-key (keyword players-arg)
        games-key (keyword games-arg)
        all (gensym "all")]      
    `(defn ~@name-doc ~[{players-arg players-key 
                         games-arg games-key
                         :as all}]
       (update-in ~all [~games-key] concat ~@body))))

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
      
(defdraw round-robin 
  "Computes a list of all games of a 
  round-robin-tournament. Each game is 
  represented by a map {:player-1 p1 :player-2 p2}, 
  which means p1 vs. p2. Being player-1 or player-2
  matters (e.g. colors in chess of home/away) 
  and is balanced for each player."
  [players games]
  (let [players-cnt (count players)
        rounds-cnt (if (even? players-cnt) 
                     (dec players-cnt) 
                     players-cnt)]
    (->> (range rounds-cnt)
      (mapcat (partial rr players-cnt))
      (map (fn [[p1 p2]] 
             {:player-1 (nth players p1) 
              :player-2 (nth players p2)})))))