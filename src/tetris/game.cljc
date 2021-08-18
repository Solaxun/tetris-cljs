(ns tetris.game)

(def rows 20)
(def cols 10)
(def board (vec (repeat rows (vec (repeat cols  " ")))))

;; pieces, along with their starting positions and rotation-center (:mid)
(def I {:type :I :mid [4 1] :coords [[3 1] [4 1] [5 1] [6 1]]})
(def J {:type :J :mid [4 1] :coords [[3 1] [4 1] [5 1] [5 0]]})
(def L {:type :L :mid [4 1] :coords [[3 1] [4 1] [5 1] [3 0]]})
(def O {:type :O :mid [4 0] :coords [[4 0] [5 0] [4 1] [5 1]]})
(def S {:type :S :mid [4 1] :coords [[4 1] [4 0] [5 0] [3 1]]})
(def T {:type :T :mid [4 1] :coords [[3 1] [4 1] [5 1] [4 0]]})
(def Z {:type :Z :mid [4 0] :coords [[4 0] [4 1] [5 1] [3 0]]})

(defn get-random-piece [] (rand-nth [I J L O S T Z]))

(defn new-game-state []
  {:board board
   :active-piece nil
   :game-over? false
   :score 0
   :level 1
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

(defn piece-fits? [board {:keys [mid coords]}]
  (let [[max-x max-y] [cols rows]
        not-occupied (fn [x y] (= " " (get-in board [y x])))]
    (every? (fn [[x y]] (and (< x max-x)
                             (< y max-y)
                             (not-occupied x y)))
            coords)))
;; need to store piece type so we know if it's an "O" which doesn't rotate :/
(defn rotate-piece [{:keys [type mid coords] :as piece}]
  (if (= type :O)
    piece
    (let [rasterized-coords (map #(map - % mid) coords)
          rotated (map (fn [[x y]] [(- y) x]) rasterized-coords)
          coords' (map #(map + mid %) rotated)]
      (assoc piece :coords coords'))))

(defn rotate-active-piece [{:keys [board active-piece] :as game-state}]
  (let [r (rotate-piece active-piece)]
    ;; TODO: duplication in this and down move, clear piece to check if fits
    ;; then clear piece again before placing (in place-piece)
    (if (piece-fits? (clear-piece board active-piece) r)
      (-> game-state (update :board place-piece active-piece r) (assoc :active-piece r))
      game-state)))

(defn move-piece [{:keys [mid coords] :as piece} dir]
  (let [dir (get {:left [-1 0] :right [1 0] :down [0 1]} dir)]
    (-> piece
        (update :coords #(mapv (partial mapv + dir) %))
        (update :mid #(mapv + % dir)))))

(defn piece-down [piece] (move-piece piece :down))
(defn piece-left [piece] (move-piece piece :left))
(defn piece-right [piece] (move-piece piece :right))

(defn make-new-rows [n]
  (vec (repeat n (vec (repeat cols " ")))))

(defn full? [row]
  (every? (partial not= " ") row))

(defn clear-rows [{:keys [board active-piece score level lines-cleared] :as game-state}]
  (let [cleared (remove full? board)
        ncleared (- rows (count cleared))]
    (assoc game-state
      :board (vec (concat (make-new-rows ncleared)
              cleared))
      :score (+ score (+ (* ncleared 100 level)
                         (if (>= ncleared 4) 500 0)))
      :lines-cleared (+ lines-cleared ncleared)
      :ghost-piece nil)))

(defn game-down [{:keys [board active-piece score] :as game-state}]
  (if (nil? active-piece)
    game-state
    (let [piece (piece-down active-piece)]
      (if (piece-fits? (clear-piece board active-piece) piece)
        (-> game-state
            (update :board place-piece active-piece piece)
            (assoc :active-piece piece))
        (assoc game-state :active-piece nil)))))

(defn hard-drop [game-state]
  ;; nil active piece means can't move further down
  (some #(when (nil? (:active-piece %1)) %1)
        (iterate game-down game-state)))

;; ghost-piece: take-while not nil and get last active piece?
(defn add-ghost-piece [game-state]
  (let [ghost (->> game-state
                   (iterate game-down)
                   (take-while #(some? (:active-piece %)))
                   last
                   :active-piece)]
    (assoc game-state :ghost-piece ghost)))

(defn spawn-piece [{:keys [board active-piece next3] :as game-state}]
  (let [spawned (first next3)]
    (if-not (piece-fits? board spawned)
      (assoc game-state :game-over? true)
      (-> game-state
          (update :board place-piece (or active-piece spawned) spawned)
          (assoc :active-piece spawned)
          (assoc :next3 (concat (rest next3) [(get-random-piece)]))
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
  (if-not (some? (:active-piece game-state))
    game-state
    (-> (case dir
          "U" (rotate-active-piece game-state)
          "D" (game-down game-state)
          "L" (game-left game-state)
          "R" (game-right game-state)
          "S" (hard-drop game-state))
      add-ghost-piece)))
