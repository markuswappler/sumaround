(ns euwe.score)

;; primitives

(defn always [x]
  (fn [player game]
    x))

(def zero (always 0))
(def one (always 1))
(def two (always 2))
(def three (always 3))

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
  `(defop ~name [f# g#]
     (when-let [x# f#]
       (when-let [y# g#]
         (~op x# y#)))))

(defbinary and? and)
(defbinary or? or)
(defbinary eq? =)
(defbinary neq? not=)
(defbinary less? <)
(defbinary greater? >)
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

(def goals-scored (choice
                    (player? :home) (game-score :home-goals)
                    (player? :away) (game-score :away-goals)))

(def goals-against (choice
                     (player? :home) (game-score :away-goals)
                     (player? :away) (game-score :home-goals)))

(def points 
  (let [diff (sub goals-scored goals-against)]
    (choice
      (greater? diff zero) three
      (less? diff zero) zero
      (eq? diff zero) one)))

(def points-total (fold + 0 points))
(def goals-scored-total (fold + 0 goals-scored))
(def goals-against-total (fold + 0 goals-against))
(def diff-total (sub goals-scored-total goals-against-total))