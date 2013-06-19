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
                   (take-while (fn [s] (not= s '&)))
                   (mapcat (fn [s] `(~s (~s ~player ~game)))))
               ~'rec (fn [& scores#] 
                       ((apply ~name scores#) ~player ~game))]
           ~body)))))

(defop choice [test then & else]
  (if test
    then
    (when else
      (apply rec else))))

(defop eq? [f g]
  (= f g))

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

(defn fold [f init score]
  (fn [player games]
    (->> games
      (map (fn [g] (score player g)))
      (filter (comp not nil?))
      (reduce f init))))

;; sample

(def ger "Germany")
(def arg "Argentina")
(def esp "Spain")

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

(def wch10 {:home "Spain"
            :away "Germany"
            :home-goals 1
            :away-goals 0})

(def games [wch86 wch90 wch10 confed])

(def goals-diff (choice
                  (player? :home) (-- (game-score :home-goals)
                                      (game-score :away-goals))
                  (player? :away) (-- (game-score :away-goals)
                                      (game-score :home-goals))))

(def points (choice
              (eq? goals-diff (always nil)) (always nil)
              (>> goals-diff (always 0)) (always 3)
              (<< goals-diff (always 0)) (always 0)
              (always true) (always 1)))

(def total (fold + 0 points))