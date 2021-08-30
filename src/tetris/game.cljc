(ns tetris.game
  (:require [tetris.rotations :as r]))

(def rows 20)
(def cols 10)
(def board (vec (repeat rows (vec (repeat cols  " ")))))

;; pieces, along with their starting positions and rotation-center (:mid)
(def I {:type :I :mid [4 1] :coords [[3 1] [4 1] [5 1] [6 1]] :rotation :SRS :facing :u})
(def J {:type :J :mid [4 1] :coords [[3 1] [4 1] [5 1] [5 0]] :rotation :SRS :facing :u})
(def L {:type :L :mid [4 1] :coords [[3 1] [4 1] [5 1] [3 0]] :rotation :SRS :facing :u})
(def O {:type :O :mid [4 0] :coords [[4 0] [5 0] [4 1] [5 1]] :rotation :SRS :facing :u})
(def S {:type :S :mid [4 1] :coords [[4 1] [4 0] [5 0] [3 1]] :rotation :SRS :facing :u})
(def T {:type :T :mid [4 1] :coords [[3 1] [4 1] [5 1] [4 0]] :rotation :SRS :facing :u})
(def Z {:type :Z :mid [4 1] :coords [[4 0] [4 1] [5 1] [3 0]] :rotation :SRS :facing :u})
(def shape->piece (apply hash-map (interleave [:I :J :L :O :S :T :Z] [I J L O S T Z])))

(defn get-random-piece [] (rand-nth [I J L O S T Z]))
(defn get-new-piece [prevpiece]
  (reduce #(if (= %1 %2) %2 (reduced %2))
          prevpiece
          (repeatedly 3 get-random-piece)))

(defn new-game-state []
  {:board board
   :active-piece nil
   :hold-piece {:piece nil :ready? true}
   :game-over? false
   :score 0
   :level 8
   :starting-level 8
   :lines-cleared 0
   :next3 (repeatedly 3 get-random-piece)})

(def game-state (atom (new-game-state)))

(defn clear-piece [board {:keys [mid coords]}]
  (reduce #(assoc-in %1 (reverse %2) " ") board coords))

(defn place-piece [board active-piece new-piece]
  (let [new-board (clear-piece board active-piece)]
    (reduce #(assoc-in %1 (reverse %2) (:type active-piece))
            new-board
            (:coords new-piece))))

(defn fresh-piece [piece] (get shape->piece (:type piece) "sumtingwong"))

(defn hold-piece
  [{:keys [board active-piece hold-piece next3] :as game-state}]
  (let [{hpiece :piece ready? :ready?} hold-piece]
    (cond (nil? hpiece)
          (-> game-state
              (update :board place-piece active-piece (first next3))
              (update :next3 #(-> % rest (concat [(get-new-piece (last next3))])))
              (assoc :active-piece (first next3))
              (assoc :hold-piece {:piece (fresh-piece active-piece) :ready? false}))

          ;; whenever a piece locks, `ready?` is set to true
          ready?
          (-> game-state
              (update :board place-piece active-piece hpiece)
              (assoc :active-piece hpiece)
              (assoc :hold-piece {:piece (fresh-piece active-piece) :ready? false}))

          :else
          game-state)))

(defn piece-fits? [board {:keys [mid coords]}]
  (let [[max-x max-y] [cols rows]
        not-occupied (fn [x y] (= " " (get-in board [y x])))]
    (every? (fn [[x y]] (and (< x max-x)
                             (< y max-y)
                             (not-occupied x y)))
            coords)))

(defn rotate-active-piece [{:keys [board active-piece] :as game-state}]
  (let [new-piece (r/rotate active-piece (clear-piece board active-piece) :r)]
    (if (piece-fits? (clear-piece board active-piece) new-piece)
      (-> game-state
          (update :board place-piece active-piece new-piece)
          (assoc :active-piece new-piece))
      game-state)))

(defn move-piece [{:keys [mid cooqrds] :as piece} dir]
  (let [dir (get {:left [-1 0] :right [1 0] :down [0 1]} dir)]
    (-> piece
        (update :coords #(mapv (partial mapv + dir) %))
        (update :mid #(mapv + % dir)))))

(defn piece-down [piece] (move-piece piece :down))
(defn piece-left [piece] (move-piece piece :left))
(defn piece-right [piece] (move-piece piece :right))

(defn make-new-rows [n]
  (vec (repeat n (vec (repeat cols " ")))))

(defn row-full? [row]
  (every? (partial not= " ") row))

(defn score-cleared [ncleared]
  ({1 100 2 300 3 500 4 800} ncleared))

(defn clear-rows [{:keys [board active-piece score level lines-cleared] :as game-state}]
  (let [cleared (remove row-full? board)
        ncleared (- rows (count cleared))]
    (assoc game-state
      :board (vec (concat (make-new-rows ncleared)
              cleared))
      :score (-> ncleared (score-cleared) (* level) (+ score))
      :lines-cleared (+ lines-cleared ncleared)
      :ghost-piece nil)))

(defn game-down
  ([{:keys [board active-piece score lock-delay?] :as game-state}]
   (game-down game-state false))
  ([{:keys [board active-piece score lock-delay?] :as game-state} soft-drop?]
   (let [piece (piece-down active-piece)]
     (if (piece-fits? (clear-piece board active-piece) piece)
       (-> game-state
           (assoc :score (if soft-drop? (inc score) score))
           (dissoc :lock-delay?)
           (update :board place-piece active-piece piece)
           (assoc :active-piece piece))
       (assoc game-state :lock-delay? true)))))

(defn hard-drop [{:keys [active-piece points score] :as game-state}]
  (let [get-max-y #(->> % :coords (map second) (apply max))
        max-y (get-max-y active-piece)]
    (some #(when (:lock-delay? %1)
             (assoc %1 :locked? true :lock-delay? false
                    :score (+ (- (get-max-y (:active-piece %)) max-y) score)))
          (iterate game-down game-state))))

(defn lock-piece [game-state]
  (hard-drop (-> game-state
                 (assoc :locked? true :lock-delay? false)
                 (update :hold-piece assoc :ready? true))))

(defn add-ghost-piece [game-state]
  (let [ghost (->> game-state
                   (iterate game-down)
                   (take-while #(not (:lock-delay? %)))
                   last
                   :active-piece)]
    (assoc game-state :ghost-piece ghost)))

(defn spawn-piece [{:keys [board active-piece next3] :as game-state}]
  (let [spawned (first next3)]
    (if-not (piece-fits? board spawned)
      (assoc game-state :game-over? true)
      (-> game-state
          (dissoc :locked?)
          (update :board place-piece spawned spawned)
          (assoc :active-piece spawned)
          (assoc :next3 (concat (rest next3) [(get-new-piece (last next3))]))
          add-ghost-piece))))

(defn game-move [{:keys [board active-piece score] :as game-state} move-func]
  (let [piece (move-func active-piece)]
    (if (piece-fits? (clear-piece board active-piece) piece)
      (-> game-state
          (update :board place-piece active-piece piece)
          (assoc :active-piece piece))
      game-state)))

(defn game-left [game-state] (game-move game-state piece-left))
(defn game-right [game-state] (game-move game-state piece-right))

(defn move [dir game-state]
  (if (:locked? game-state)
    game-state
    (-> (case dir
          "U" (rotate-active-piece game-state)
          "D" (game-down game-state true)
          "L" (game-left game-state)
          "R" (game-right game-state)
          "S" (hard-drop game-state)
          "C" (hold-piece game-state))
      add-ghost-piece)))
