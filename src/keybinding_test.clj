(ns keybinding-test
  (:require [seesaw.core :as sc]
            [seesaw.keymap :as skm]
            [seesaw.keystroke :as sks])

  (:import (java.awt.event ActionEvent)))

(defn new-frame []
  (let [label (sc/label :text "START", :font {:size 100})

        frame (sc/frame :content label, :size [1000 :by 1000])]

    (skm/map-key label (sks/keystroke "DOWN")
                 (fn [^ActionEvent e]
                   (println e))
                 :scope :global)

    frame))