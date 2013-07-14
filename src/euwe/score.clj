(ns euwe.score)

;; primitives

(defn always [x]
  (fn [player game]
    x))

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
        part (fn [k] (->> body
                       (partition 2)
                       (filter #(= k (first %)))
                       (map second)
                       first))
        depend (part :depend)
        depend (cond
                 (nil? depend) nil
                 (vector? depend) depend
                 :else (vector depend))
        player (part :player)
        score (part :score)
        scr-names (take-nth 2 score)
        yield (part :yield)]
    (if player
      `(defn ~name [~players ~games]
         (let [~@(mapcat
                   (fn [d] `(~d (~d ~players ~games))) 
                   depend)
               ~@score]
           (for [~player ~players
                 :let [~@(mapcat 
                           (fn [s] `(~s (~s ~player ~games))) 
                           scr-names)]]
             ~yield)))
      `(defn ~name [~games]
         (let [~@score
               ~@(mapcat
                   (fn [s] `(~s (~s nil ~games)))
                   scr-names)]
           ~yield)))))