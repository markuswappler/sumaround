(ns euwe.score)

;; primitives

(defn always [x]
  (fn [player game]
    x))

(defn game-score [f]
  (fn [player game]
    (f game)))

(defn player? [f]
  (fn [player game]
    (= player (f game))))

;; combinators

(defmacro defop [name scores body]
  (let [player (gensym "player")
        game (gensym "game")]
    `(defn ~name ~scores
       (fn [~player ~game]
         (let [~@(->> scores
                   (take-while (fn [scr] (not= scr (symbol "&"))))
                   (mapcat (fn [scr] `(~scr (~scr ~player ~game)))))
               ~(symbol "rec") (fn [& scores#] 
                                 ((apply ~name scores#) ~player ~game))]
           ~body)))))

(defop choice [test then & else]
  (if test
    then
    (when else
      (apply rec else))))

(defop >> [f g]
  (> f g))

(defop << [f g]
  (< f g))

(defop ++ [f g]
  (+ f g))

(defop -- [f g]
  (- f g))

(defop mult [f g]
  (* f g))

(defop div [f g]
  (/ f g))

;; sample

(def ger "Germany")
(def arg "Argentina")
(def wch86 {:home "Germany"
            :away "Argentina"
            :home-goals 2
            :away-goals 3})
(def wch90 {:home "Germany"
            :away "Argentina"
            :home-goals 1
            :away-goals 0})
(def confed {:home "Germany"
             :away "Argentina"
             :home-goals 2
             :away-goals 2})

(def goals-diff (choice
                  (player? :home) (-- (game-score :home-goals)
                                      (game-score :away-goals))
                  (player? :away) (-- (game-score :away-goals)
                                      (game-score :home-goals))))

(def points (choice
              (>> goals-diff (always 0)) (always 3)
              (<< goals-diff (always 0)) (always 0)
              (always true) (always 1)))