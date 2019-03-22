(ns snake-redo.ui.main
  (:require [seesaw.core :as sc]
            [seesaw.keystroke :as sks]
            [seesaw.keymap :as skm]

            [snake-redo.ui.canvas :as canv]
            [snake-redo.logic.game-state :as gs]
            [snake-redo.logic.world :as w]
            [snake-redo.ui.helpers :as uih]
            [snake-redo.logic.ai :as ai]

            [helpers.general-helpers :as g]

            [clojure.string :as s])

  (:import [java.util.concurrent ExecutorService]))

; TODO: Prevent the user from accidentally killing self by causing the snake to backtrack

(def settings-font
  {:size 50, :name "Arial"})

(def global-rand-gen
  (g/new-rand-gen))

(def starting-dimensions [50 50])

(defn new-random-starting-state [dimensions rand-gen]
  (gs/new-game-state
    (w/new-random-centered-world dimensions rand-gen)))

(def game-advance-delay
  30)

(defn ai-decider-f [world]
  (let [depth (->> world
                   :dimensions
                   (apply max))]

    (ai/straight-direction-scanner world depth)))

(defn parse-size [size-str]
  (let [num? #(Character/isDigit ^Character %)
        [width rest-half] (split-with num? size-str)
        [_ height] (split-with #(not (num? %)) rest-half)
        parsed (mapv #(g/parse-int (apply str %)) [width height])]

    (when (every? identity parsed)
      parsed)))

(defn reset-state-according-to-settings! [state-atom settings-panel]
  (let [size-input (sc/select settings-panel [:#size-input])]
    (when-let [dims (parse-size (sc/text size-input))]
      (reset! state-atom (new-random-starting-state dims global-rand-gen)))))

(defn new-reset-button [settings-panel state-atom]
  (let [button (sc/button :text "Reset", :font settings-font)]

    (sc/listen button :action
               (fn [_] (reset-state-according-to-settings! state-atom settings-panel)))

    button))

(defn new-settings-panel [state-atom]
  (let [size-row (sc/horizontal-panel
                   :items [(sc/label "Width x Height")
                           (sc/text :text "", :font settings-font, :id :size-input)])

        tweak-panel (sc/vertical-panel :items [(sc/horizontal-panel :items [size-row])])
        reset-button (new-reset-button tweak-panel state-atom)

        settings-panel (sc/border-panel :center tweak-panel
                                        :south reset-button

                                        :id :settings-root)]

    settings-panel))


(defn new-main-panel [state-atom]
  (let [canvas (canv/new-canvas state-atom)
        settings-panel (new-settings-panel state-atom)

        main-panel (sc/border-panel :center canvas,
                                    :south settings-panel)]

    main-panel))

(defn start-game-advancer [state-atom canvas advance-delay]
  (uih/start-stoppable-timer
    (fn []
      (swap! state-atom gs/advance-state global-rand-gen)
      (sc/repaint! canvas))

    advance-delay))

; TODO: Just switch to ScheduledService?
; Will ignore any changes made to the state between the time it starts thinking
;  and the time it makes a decision. Realisitic?
(defn start-ai-updater [ai-decider state-atom update-delay]
  (let [^ExecutorService pool (uih/new-daemon-pool 1)]
    (uih/start-stoppable-timer
      (fn []
        (.execute pool
          (fn []
            (let [current-state @state-atom
                  new-direction (ai-decider (:world current-state))]
              (swap! state-atom gs/change-direction new-direction)))))

      update-delay)))

(defn bind-keys [target keys action]
  (doseq [k keys]
    (skm/map-key target
                 (sks/keystroke k)
                 (fn [_] (action))
                 :scope :global)))


(defn attach-key-mapping [canvas settings-panel state-atom]
  (letfn [(change-dir [dir] (swap! state-atom gs/change-direction dir))
          (dir-map [& pairs] (->> pairs
                                  (partition 2)
                                  (mapv (fn [[keys dir]] (bind-keys canvas keys
                                                                    (fn [] (change-dir dir)))))))]

    (dir-map
      [\w "UP"] [0 -1]
      [\s "DOWN"] [0 1]
      [\a "LEFT"] [-1 0]
      [\d "RIGHT"] [1 0])

    (bind-keys canvas [\r] #(reset-state-according-to-settings! state-atom settings-panel))))

(defn new-frame [state-atom]
  (let [main-panel (new-main-panel state-atom)
        canvas (sc/select main-panel [:#canvas])
        settings-panel (sc/select main-panel [:#settings-root])

        frame (sc/frame :content main-panel, :size [1000 :by 1000])

        stop-advancer-f (start-game-advancer state-atom canvas game-advance-delay)]

    (attach-key-mapping canvas settings-panel state-atom)

    (sc/listen frame :window-closing
       (fn [_]
         (stop-advancer-f)))

    frame))

(defn -main
  "Expects the string AI or PLAYER, indicating who's playing."
  [& [player-option]]
  (let [state-atom (atom (new-random-starting-state starting-dimensions global-rand-gen))

        ai? (or (nil? player-option)
                (= (first (s/lower-case player-option)) \a))

        stop-playing-f (if ai?
                         (start-ai-updater ai-decider-f state-atom (int (/ game-advance-delay 2)))
                         (fn [] "Dummy stop function.")) ; TODO: Smell?

        frame (new-frame state-atom)]

    (sc/listen frame :window-closing
       (fn [_]
         (stop-playing-f)))

    (-> frame
        (sc/show!))))

