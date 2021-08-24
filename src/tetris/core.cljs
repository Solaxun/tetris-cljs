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

(declare start-game)

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
   [:button {:on-click #(do (reset! state (game/spawn-piece (game/new-game-state)))
                            (start-game @state))
             :style    {:height "50px" :width "100px"}}
    "New Game"]])

;; -------------------------
;; Initialize app

(def moves-chan (async/chan))
(def lock-delay-ms 500)
(defn new-grav-chan []
  (let [level (:level @state)
        ms-delay (-> (- level 1)
                     (* 0.007)
                     (#(- 0.8 %))
                     (Math/pow (- level 1))
                     (* 1000)
                     (Math/round))]
    (async/timeout ms-delay)))

(defn process-move [e]
  (let [k (.-keyCode e)
        dir ({32 "S" 39 "R" 37 "L" 40 "D" 38 "U"} k)]
    (when (some #{dir} ["R" "L" "U" "D" "S"])
      (async/put! moves-chan dir))))

(defn update-score [{:keys [score level lines-cleared] :as state}]
  (let [a (* level 10)
        b (max 100 (- (* level 10) 50))] ; questionable if this is needed
    (update state :level (fn [l] (if (>= lines-cleared a #_(min a b))
                                  (inc level)
                                  level)))))

(defn mount-root []
  (d/render
    [home-page]
    (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root)
  (.addEventListener js/window "keydown" process-move))

(defn lock-delay []
  (async/go-loop [lock-delay (async/timeout lock-delay-ms)
                  total-delay (async/timeout 3000)
                  gravity (new-grav-chan)]
    (let [[v c] (alts! [lock-delay total-delay moves-chan gravity])]
      (cond (= c total-delay) (swap! state game/lock-piece)
            (= c lock-delay)  (swap! state game/lock-piece)
            (= c gravity) (let [s (swap! state game/game-down)]
                            ;; if unable to move down, lock delay still present
                            (if (:lock-delay? s)
                              (recur lock-delay total-delay
                                     (new-grav-chan))
                              ;; successful moves down (gravity or soft drop)
                              ;; removes lock delay and we continue as normal
                              s))
            :else (do (swap! state #(game/move v %))
                      ;; TODO: do we need to also remove lock delay on soft-drop
                      ;; down moves, or is gravity enough?
                      ;; I think yes bc with longer gravity if you tap down inside
                      ;;the gravity interval you could get a sudden hard-drop after 3 sec

                      ;; lock delay calibrated near gravity speed? If grav is 10 sec
                      ;; then you will always hard drop when moving/rotating for 3 secs
                      ;; if they are close together, maybe don't need to reset on soft drop?
                      (recur (async/timeout lock-delay-ms) total-delay gravity))))))

(defn start-game
  [gs]
  (async/go-loop [{:keys [board active-piece game-over? level lock-delay? locked?]} gs
                  grav-chan (new-grav-chan)]
    (cond game-over?
          (do (.alert js/window "game over!") board)

          lock-delay?
          (recur (<! (lock-delay)) (new-grav-chan))
          ;; without nil check each move will clear while piece still moving
          (and locked? (some game/row-full? board))
          (recur (swap! state (fn [s] (-> s game/clear-rows update-score))) grav-chan)

          locked?
          (recur (swap! state game/spawn-piece) grav-chan)

          :else
          (let [[move c] (async/alts! [grav-chan moves-chan])]
            (if (= c grav-chan)
              (recur (swap! state game/game-down) (new-grav-chan))
              (recur (swap! state #(game/move move %)) grav-chan))))))

(start-game @state)
