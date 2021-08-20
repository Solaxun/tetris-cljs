(ns tetris.core
  (:require
    [reagent.core :as r]
    [reagent.dom :as d]
    [tetris.game :as game]
    [clojure.core.async :as async]))

;; -------------------------
;; Views
(def state (r/atom (game/spawn-piece (game/new-game-state))))
(def piece-colors
  {:I "#00ffff"
   :J "#FF971C"
   :L "#2600ff"
   :O "#FFD500"
   :S "#00ff00"
   :T "#FF00FF"
   :Z "#ff0000"})

(defn pending-pieces []
  [:table {:style {:background-color "black" :margin-bottom "20px" :border-collapse "collapse"}}
   (let [queued (:next3 @state)
         qboard (vec (repeat 9 (vec (repeat 9 " "))))]
     ;;TODO: place each piece in the 3x3 grid and call this in home-page
     (for [[i row] (map-indexed vector qboard)]
       [:tr
        (for [[j cell] (map-indexed vector row)]
          [:td {:style {:width            "30px"
                        :background-color (piece-colors cell)
                        :height           "30px"
                        :border           "solid"
                        :border-width     "1px"}}])]))])

(defn home-page []
  [:center [:div {:style {:font-size 30 :color "black" :margin-bottom "20px"}}
            "Level:" (@state :level) [:br] [:br]
            "Score:" (@state :score) [:br] [:br]
            "Cleared:" (@state :lines-cleared)]
   [:div (map #(:type %) (:next3 @state))]
   [:table {:style {:background-color "black" :margin-bottom "20px" :border-collapse "collapse"}}
    (let [{board :board ghost :ghost-piece {piece-type :type coords :coords} :active-piece} @state]
      (for [[i row] (map-indexed vector board)]
        [:tr
         (for [[j cell] (map-indexed vector row)]
           [:td {:style {:width            "30px"
                         :background-color (if (and (some #{[j i]} (:coords ghost))
                                                 (= cell " "))
                                             "grey"
                                             (piece-colors cell))
                         :height           "30px"
                         :border           "solid"
                         :border-width     "1px"}}])]))]
   [:button {:on-click #(        reset! state (game/spawn-piece (game/new-game-state)))
             :style    {:height "50px" :width "100px"}}
    "New Game"]])

;; -------------------------
;; Initialize app

(def moves-chan (async/chan))

(defn process-move [e]
  (let [k (.-keyCode e)
        dir ({32 "S" 39 "R" 37 "L" 40 "D" 38 "U"} k)]
    (when (some #{dir} ["R" "L" "U" "D" "S"])
      (async/put! moves-chan dir))))

(defn score->level [_ score]
  (condp >= score
    500 1
    2000 2
    7000 3
    15000 4
    30000 5
    50000 6
    7))

(defn update-score [{:keys [score level] :as state}]
  (update state :level (fn [l] (score->level level  score))))

(defn mount-root []
  (d/render
    [home-page]
    (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root)
  (.addEventListener js/window "keydown" process-move))

(defn lock-delay []
  (async/go-loop [lock-delay (async/timeout 250)
                  total-delay (async/timeout 3000)
                  gravity (async/timeout (- 450 (* 50 (:level @state))))]
    (let [[v c] (alts! [lock-delay total-delay moves-chan gravity])]
      (cond (= c total-delay) (swap! state game/lock-piece)
            (= c lock-delay)  (swap! state game/lock-piece)
            (= c gravity) (do (swap! state game/game-down)
                              (recur lock-delay total-delay
                                     (async/timeout (- 450 (* 50 (:level @state))))))
            :else (do (swap! state #(game/move v %))
                      (recur (async/timeout 250) total-delay gravity))))))

(defn game-tick
  [gs]
  (async/go-loop [{:keys [board active-piece game-over? level lock-delay? locked?]} gs
                  grav-chan (async/timeout (- 450 (* 50 level)))]
    (cond game-over?
          (do (.alert js/window "game over!") board)

          lock-delay?
          (recur (<! (lock-delay)) (async/timeout (- 450 (* 50 level))))
          ;; without nil check each move will clear while piece still moving
          (and locked? (some game/row-full? board))
          (recur (swap! state (fn [s] (-> s game/clear-rows update-score))) grav-chan)

          locked?
          (recur (swap! state game/spawn-piece) grav-chan)

          :else
          (let [[move c] (async/alts! [grav-chan moves-chan])]
            (if (= c grav-chan)
              (recur (swap! state game/game-down) (async/timeout (- 450 (* 50 level))))
              (recur (swap! state #(game/move move %)) grav-chan))))))

(game-tick @state)




;; if down move (non hard-drop) is blocked, start the lock delay as a
;; go block which we wait for by taking from the resultant channel returned
;; from that block.

;; then in the block, do the lock delay e.g. 3000/250ms updates until locked
;; then the go block will return to the game-loop where we should start gravity
;; over and respawn, by recurring or otherwise
