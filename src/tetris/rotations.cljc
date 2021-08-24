(ns tetris.rotations)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://tetris.wiki/Super_Rotation_System

;; kicks are (x,y) pairs where positive "x" values are moves to the right
;; and positive "y" values are upward moves - so standard (x,y) graph coords
;; since we are treating positive "y" values as row-moves (downward), we need
;; to negate these values.

;; the kicks apply to the whole piece, e.g. a kick of [1,2] means add that amt
;; to every element of the piece coordinate vector

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def cols 10)
(def rows 20)

(def kick-table
  {:u {:r [[0 0] [-1 0] [-1 1] [0 -2] [-1 -2]]
       :l [[0 0] [1 0] [1 1] [0 -2] [1 -2]]}
   :r {:r [[0 0] [1 0] [1 -1] [0 2] [1 2]]
       :l [[0 0] [1 0] [1 -1] [0 2] [1 2]]}
   :d {:r [[0 0] [-1 0] [-1 1] [0 -2] [-1 -2]]
       :l [[0 0] [1 0] [1 1] [0 -2] [1 -2]]}
   :l {:r [[0 0] [-1 0] [-1 -1] [0 2] [-1 2]]
       :l [[0 0] [-1 0] [-1 -1] [0 2] [-1 2]]}})

(def kick-table-I
  {:u {:r [[0 0] [-2 0] [1 0] [-2 -1] [1 2]]
       :l [[0 0] [-1 0] [2 0] [-1 2] [2 -1]]}
   :r {:r [[0 0] [-1 0] [2 0] [-1 2] [2 -1]]
       :l [[0 0] [2 0] [-1 0] [2 1] [-1 -2]]}
   :d {:r [[0 0] [1 0] [-2 0] [1 -2] [-2 1]]
       :l [[0 0] [2 0] [-1 0] [2 1] [-1 -2]]}
   :l {:r [[0 0] [1 0] [-2 0] [1 -2] [-2 1]]
       :l [[0 0] [-2 0] [1 0] [-2 -1] [1 2]]}})

(defn clear-piece [board {:keys [mid coords]}]
  (reduce #(assoc-in %1 (reverse %2) " ") board coords))

(defn piece-fits? [board {:keys [mid coords]}]
  (let [[max-x max-y] [cols rows]
        _ (println max-x max-y cols rows)
        not-occupied (fn [x y] (= " " (get-in board [y x])))]
    (every? (fn [[x y]] (and (< x max-x)
                            (< y max-y)
                            (not-occupied x y)))
            coords)))

(defn basic-rotate-right [{:keys [type mid coords facing] :as piece}]
  (if (= type :O)
    piece
    (let [rasterized-coords (map #(map - % mid) coords)
          rotated (map (fn [[x y]] [(- y) x]) rasterized-coords)
          coords' (map #(map + mid %) rotated)]
      (assoc piece
             :coords coords'
             :facing (get {:u :r :d :l :l :u :r :d} facing)))))

(defn kick
  "negate the y value since the kick-tables are standard x/y graph coords
  where +x is right and +y is up, but our representation treats +y as a move
  down e.g. rows."
  [piece [xkick ykick]]
  (-> piece
      (update :coords (fn [cs] (mapv (fn [c] (mapv + c [xkick (- ykick)])) cs)))
      (update :mid    (fn [m]  (mapv + m [xkick (- ykick)])))))

(defmulti rotate
  (fn [piece board dir]
    [(:type piece) (:rotation piece)]))

(defmethod rotate [:I :SRS]
  [{:keys [coords mid facing] :as piece} board dir]
  (let [kicks (get-in kick-table-I [facing dir])
        new-piece (some #(when (piece-fits? board %) %)
                        (map (fn [k] (kick (basic-rotate-right piece) k))
                             kicks))]
    (or new-piece piece)))

(defmethod rotate [:O :SRS]
  [piece board dir]
  piece)

(defmethod rotate :default
  [{:keys [coords mid facing] :as piece} board dir]
  (let [kicks (get-in kick-table [facing dir])
        possible-pieces (map (fn [k] (kick (basic-rotate-right piece) k)) kicks)
        #_#_ _ (println possible-pieces)
        new-piece (some #(when (piece-fits? board %) %) possible-pieces)
        #_#_ _ (println new-piece)]
    (or new-piece piece)))
