(ns tetris.core
  (:require
    [reagent.core :as r]
    [reagent.dom :as d]
    [tetris.game :as game]))

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
   [:button {:on-click #(reset! state (game/spawn-piece (game/new-game-state)))
             :style    {:height "50px" :width "100px"}}
    "New Game"]])

(defn process-move [e]
  (let [k (.-keyCode e)
        dir ({32 "S" 39 "R" 37 "L" 40 "D" 38 "U"} k)]
    (when (some #{dir} ["R" "L" "U" "D" "S"])
      (swap! state (partial game/move dir)))))

;; -------------------------
;; Initialize app
(def msg (atom 0))
#_(def ^:mutable gameloop)

(defn score->level [_ score]
  (condp >= score
    500 1
    1000 2
    2000 3
    5000 4
    10000 5
    20000 6
    7))
;; we have about 0.2 seconds to move until lock, and 3 seconds total of delaying the lock
;; so, when piece touches, if you don't move in 0.2 seconds, it locks, but you're still on
;; an overall timer of 3 seconds where it will lock regardless of how much you try moving
(defn game-tick
  [game-state]
  (let [{:keys [board active-piece game-over? score]} @game-state]
    #_(.log js/console (:coords active-piece))
    (if game-over?
      (when (zero? @msg)
        (swap! msg inc)
        (.alert js/window "game over!")
        #_(.clearInterval gameloop)
        board)
      (do (when (some game/full? board)
            (swap! game-state game/clear-rows))
          (when (nil? active-piece)
            (swap! game-state game/spawn-piece))
          (swap! game-state update :level score->level score)
          (swap! game-state #(if (:game-over? %) % (game/game-down %)))))))

(defn dotick []
  (.setTimeout js/window
    #(do (game-tick state) (when-not (:game-over @state) (dotick)))
    (- 250 (-> @state :level (* 50)))))

(dotick)
#_(.setInterval js/window #(game-tick state) (/ 1000 (@state :level)))

(defn mount-root []
  (d/render
    [home-page]
    (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root)
  (.addEventListener js/window "keydown" process-move))

#_(go-loop [lock-delay (async/timeout 250)
          total-delay (async/timeout 3000)
          moves  move-chan]
  (let [[v c] (alts! [lock-delay total-delay moves])]
    (cond (= c total-delay) ?
          (= c lock-delay ?)
          (= c moves ?))))
