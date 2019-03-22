(ns snake-redo.ui.canvas
  (:require [seesaw.core :as sc]
            [seesaw.graphics :as sg]
            [seesaw.font :as sf]

            [snake-redo.ui.helpers :as uih]
            [snake-redo.logic.world :as w]
            [snake-redo.logic.ai :as ai]
            [snake-redo.logic.game-state :as gs]
            [snake-redo.logic.snake :as snake]

            [helpers.general-helpers :as g])
  (:import (java.awt Component)))

(def lose-font (sf/font :name "Arial", :size 50))
(def lose-text "You Died")

(defn world->canvas-position [position world-dimensions canvas-dimensions]
  (let [[x y] position
        [cw ch] canvas-dimensions
        [ww wh] world-dimensions]

    [(g/map-range x, 0 ww, 0 cw)
     (g/map-range y, 0 wh, 0 ch)]))

(defn cell-dimensions [world-dimensions canvas-dimensions]
  (mapv #(long (/ % %2))
        canvas-dimensions
        world-dimensions))

(defn paint [state-atom canvas g]
  (let [{:keys [world status]} @state-atom
        {:keys [snake dimensions food]} world
        canvas-dims (uih/component-dimensions canvas)
        [cell-width cell-height] (cell-dimensions dimensions canvas-dims)

        draw-cell #(sg/rect % %2 cell-width cell-height)
        w->c-pos #(world->canvas-position % dimensions canvas-dims)

        [fx fy] (w->c-pos food)

        [hx hy] (w->c-pos (snake/get-head snake))

        score-size 100]

    (doseq [tail-pos (snake/get-tail snake)
            :let [[cx cy] (world->canvas-position tail-pos dimensions canvas-dims)]]

      (sg/draw g
         (draw-cell cx cy)
         (sg/style :background :black)))

    (sg/draw g
       (draw-cell fx fy)
       (sg/style :background :orange)

       (draw-cell hx hy)
       (sg/style :background :red)

       (sg/string-shape score-size score-size (str (snake/length snake)))
       (sg/style :font {:size score-size}))

    (when (= status ::gs/lost)
      (uih/draw-centered-text canvas g lose-font lose-text))))

(defn new-canvas [state-atom]
  (let [canvas (sc/canvas :paint (partial paint state-atom), :id :canvas)]

    ; So we can steal focus back from text boxes using a click
    (sc/listen canvas
       :mouse-released (fn [_]
                         (when-not (.requestFocusInWindow ^Component canvas)
                           (println "Refocus failed!"))))

    canvas))

(defn test-main [depth]
 (let [world (w/new-world [0 0] [75 75] [2 2])
       state-atom (atom (gs/new-game-state world))
       canvas (new-canvas state-atom)
       frame (sc/frame :content canvas, :size [1600 :by 1600])

       cont?-atom (atom true)]

   (sc/listen frame :window-closing
              (fn [_]
                (println "Killing...")
                (reset! cont?-atom false)))

   (sc/show! frame)

   (let [r (g/new-rand-gen)]
     ;(read-line)
     (doseq [n (range)
             :while @cont?-atom]

       (let [new-state (swap! state-atom
                         #(-> %
                              (update :world
                                      (fn [w]
                                        (w/change-snake-direction w (ai/straight-direction-scanner w depth))))

                              (gs/advance-state r)))]

         (when (gs/lost? new-state)
           (reset! cont?-atom false)))

       (sc/repaint! canvas)

       (Thread/sleep 1))

     (println "Ended..."))))

