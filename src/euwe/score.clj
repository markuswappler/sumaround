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

;; combinators

(defmacro defoperation [name scores body]
  (let [player (gensym "player")
        game (gensym "game")]
    `(defn ~name ~scores
       (fn [~player ~game]
         (let [~@(->> scores
                   (take-while (fn [s] (not= s '&)))
                   (mapcat (fn [s] `(~s (~s ~player ~game)))))
               ~'rec (fn [& scores#] 
                       ((apply ~name scores#) ~player ~game))]
           ~body)))))

(defoperation choice [test then & else]
  (if test
    then
    (when else
      (apply rec else))))

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
(defbinary add +)
(defbinary sub -)
(defbinary mult *)
(defbinary div /)

(defn fold [f init score]
  (fn [player games]
    (->> games
      (map (fn [g] (score player g)))
      (filter (comp not nil?))
      (reduce f init))))

(defn sum-up [score]
  (fold + 0 score))

(defmacro deftable [name [playersymb] bindings body]
  (let [players (gensym "players")
        games (gensym "games")        
        scorer-names (take-nth 2 bindings)]
    `(defn ~name [~players ~games]
       (let [~@bindings]
         (for [~playersymb ~players
               :let [~@(mapcat 
                         (fn [s] `(~s (~s ~playersymb ~games))) 
                         scorer-names)]]
       ~body)))))