(ns euwe.score)

;; primitives

(defn always [x]
  (fn [player game] x))

(def yes (always true))
(def no (always false))

(def zero (always 0))
(def one (always 1))
(def two (always 2))
(def three (always 3))
(def half (always 1/2))

(defn game-score [f]
  (fn [player game] 
    (f game)))

(defn player? [f]
  (fn [player game]
    (= player (f game))))

(defn lookup [table player-fn score-fn]
  (fn [player game]
    (->> table
      (some (fn [row] 
              (when (= player (player-fn row)) 
                row)))
      score-fn)))

;; combinators

(defmacro defoperation [name scores body]
  (let [player (gensym "player")
        game (gensym "game")]
    `(defn ~name ~scores
       (fn [~player ~game]
         (let [~@(->> scores
                   (take-while (fn [s] (not= s '&)))
                   (mapcat (fn [s] `(~s (~s ~player ~game)))))
               ~name (fn [& scores#] 
                       ((apply ~name scores#) ~player ~game))]
           ~body)))))

(defoperation choice [test then & else]
  (if test
    then
    (when else
      (apply choice else))))

(defmacro defbinary [name op]
  `(defoperation ~name [f# g#]
     (when-let [x# f#]
       (when-let [y# g#]
         (~op x# y#)))))

(defbinary and? and)
(defbinary or? or)
(defbinary eq? =)
(defbinary neq? not=)
(defbinary lt? <)
(defbinary gt? >)
(defbinary plus +)
(defbinary minus -)
(defbinary mult *)
(defbinary div /)

(defn player-> [pred score]
  (fn [player game]
    (score (pred player game) game)))

(defn fold [f init score]
  (fn [player games]
    (->> games
      (map (fn [g] (score player g)))
      (filter (comp not nil?))
      (reduce f init))))

(defn sum [score]
  (fold + 0 score))

;; execution

(defmacro deftable [name & body]
  (let [players (gensym "players")
        games (gensym "games")
        parts (update-in 
                (apply hash-map body)
                [:depend]
                (fn [d] (cond 
                          (nil? d) nil
                          (vector? d) d 
                          :else (vector d))))]
    (if (parts :player)
      `(defn ~name [~players ~games]
         (let [~@(mapcat
                   (fn [d] `(~d (~d ~players ~games))) 
                   (parts :depend))
               ~@(parts :score)]
           (for [~(parts :player) ~players
                 :let [~@(mapcat 
                           (fn [s] `(~s (~s ~(parts :player) ~games))) 
                           (take-nth 2 (parts :score)))]]
             ~(parts :yield))))
      `(defn ~name [~games]
         (let [~@(parts :score)
               ~@(mapcat
                   (fn [s] `(~s (~s nil ~games)))
                   (take-nth 2 (parts :score)))]
           ~(parts :yield))))))