(ns tetris.core
  (:require
    [reagent.core :as r]
    [reagent.dom :as d]
    [tetris.game :as game]
    [clojure.core.async :as async]))

;; -------------------------
;; Views
(defonce state (r/atom (game/spawn-piece (game/new-game-state))))

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
   [:div (get-in @state [:hold-piece :piece :type])]
   [:table {:style {:background-color "black" :margin-bottom "20px" :border-collapse "collapse"}}
    (let [{board :board
           ghost :ghost-piece
           hold  :hold-piece
           {piece-type :type coords :coords} :active-piece} @state]
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
(def level->gravms
  (let [gravf #(-> (- % 1)
                   (* 0.007)
                   ((fn [x] (- 0.8 x)))
                   (Math/pow (- % 1))
                   (* 1000)
                   (Math/round))]
    (apply hash-map (mapcat #(vector %1 (gravf %1))
                            (range 1 21)))))

(defn new-grav-chan []
  (let [level (:level @state)
        ms-delay (level->gravms level)]
    (async/timeout ms-delay)))

(defn process-move [e]
  (let [k (.-keyCode e)
        dir ({32 "S" 39 "R" 37 "L" 40 "D" 38 "U" 67 "C"} k)]
    (when (some #{dir} ["R" "L" "U" "D" "S" "C"])
      (async/put! moves-chan dir))))

(defn update-score [{:keys [starting-level lines-cleared] :as state}]
  (assoc state :level (+ starting-level (quot lines-cleared 10))))

(defn mount-root []
  (d/render
    [home-page]
    (.getElementById js/document "app")))

(defn ^:export init! []
  (mount-root)
  (.addEventListener js/window "keydown" process-move))

(defn progress-made? [sold snew]
  (not= (-> sold :active-piece :coords)
        (-> snew :active-piece :coords)))

(defn lock-delay []
  (async/go-loop [lock-delay (async/timeout lock-delay-ms)
                  total-delay (async/timeout 3000)
                  gravity (new-grav-chan)]
    (let [[v c] (alts! [lock-delay total-delay moves-chan gravity])]
      (cond (= c total-delay) (swap! state game/lock-piece)
            (= c lock-delay)  (swap! state game/lock-piece)
            (= c gravity) (let [s (swap! state game/game-down)]
                            (if (:lock-delay? s)
                              ;; unable to move down - lock delay still present
                              (recur lock-delay total-delay
                                     (new-grav-chan))
                              ;; successful gravity move removes lock delay
                              s))
            ;; if move is hard drop, return rather than recur
            ;; for other moves, don't reset if no progress made (e.g. stuck
            ;; against wall or other pieces), only reset when moves results
            ;; in a new position
            :else (let [[s s'] (swap-vals! state #(game/move v %))]
                    ;; lock delay calibrated near gravity speed? If grav is 10 sec
                    ;; then you will always hard drop when moving/rotating for 3 secs
                    ;; if they are close together, maybe don't need to reset on soft drop?

                    ;; only successful moves reset the delay - and up to a max of 4 times
                    ;; to prevent continually spinning.
                    (if (:locked? s)
                      s
                      (recur (if (progress-made? s s')
                               (async/timeout lock-delay-ms)
                               lock-delay)
                             total-delay
                             gravity)))))))

(defn start-game
  [gs]
  (async/go-loop [{:keys [board active-piece game-over? level lock-delay? locked?]} gs
                  grav-chan (new-grav-chan)]
    (cond game-over?
          (do (.alert js/window "game over!") board)

          lock-delay?
          (recur (<! (lock-delay)) (new-grav-chan))

          (and locked? (some game/row-full? board))
          (recur (swap! state (comp update-score game/clear-rows)) grav-chan)

          locked?
          (recur (swap! state game/spawn-piece) grav-chan)

          :else
          (let [[move c] (async/alts! [grav-chan moves-chan])]
            (if (= c grav-chan)
              (recur (swap! state game/game-down) (new-grav-chan))
              (recur (swap! state #(game/move move %)) grav-chan))))))

(start-game @state)
