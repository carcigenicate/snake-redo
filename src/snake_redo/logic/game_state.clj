(ns snake-redo.logic.game-state
  (:require [snake-redo.logic.world :as w]))

(def statuses #{::playing ::lost})

(defn new-game-state
  ([starting-world starting-status]
   {:world starting-world, :status starting-status})

  ([starting-world]
   (new-game-state starting-world ::playing)))

(defn playing? [state]
  (= (:status state) ::playing))

(defn lost? [state]
  (= (:status state) ::lost))

(defn advance-state [state rand-gen]
  (let [{:keys [world status]} state]
    (case status
      ::playing (if-let [new-world (w/advance world rand-gen)]
                  (assoc state :world new-world)
                  (assoc state :status ::lost))

      ::lost state)))

(defn change-direction [state new-direction]
  (update state :world w/change-snake-direction new-direction))

(defn score [state]
  (-> state
      (get-in [:world :snake :body])
      (count)))