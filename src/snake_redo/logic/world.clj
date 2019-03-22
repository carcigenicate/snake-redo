(ns snake-redo.logic.world
  (:require [snake-redo.logic.snake :as snake]
            [snake-redo.helpers :as snh]
            [clojure.string :as s]
            [helpers.general-helpers :as g]))

(def legal-moves (set (snh/possible-moves)))

(defn new-world [starting-snake-position dimensions starting-food-position]
  {:snake (snake/new-snake starting-snake-position),
   :dimensions dimensions,
   :food starting-food-position})

(defn new-random-centered-world [dimensions rand-gen]
  (let [half-dims (mapv #(int (/ % 2)) dimensions)
        rand-food (mapv #(g/random-int 0 % rand-gen) dimensions)]
    (new-world half-dims dimensions rand-food)))

(defn change-snake-direction [world new-direction]
  (if (legal-moves new-direction)
    (update world :snake snake/change-direction new-direction)

    (throw (IllegalStateException.
             (str "Illegal Move: " new-direction)))))

(defn checked-change-snake-direction
  "Returns nil if the new direction would cause the snake to immediately collide with its neck."
  [world new-direction]
  (let [{:keys [snake]} world]
    (when-let [changed (snake/checked-changed-directions snake new-direction)]
      (assoc world :snake changed))))

(defn snake-can-eat-food? [snake food]
  (= (snake/get-head snake)
     food))

(defn handle-snake-movement
  "Advances the snake by one tick according to its internal direction.
  Returns nil if the snake collided with itself."
  [world]
  (let [{:keys [snake dimensions]} world
        moved-snake (snake/move-in-direction snake)]

    (when-not (snake/head-overlapping-body? moved-snake)
      (assoc world :snake (snake/wrap-snake-ends moved-snake dimensions)))))

(defn randomize-food [world rand-gen]
  (let [{:keys [dimensions]} world]
    (assoc world :food (snh/random-point-in dimensions rand-gen))))

(defn handle-eating [world rand-gen]
  (let [{:keys [snake food]} world]
    (if (snake-can-eat-food? snake food)
      (-> world
          (randomize-food rand-gen)
          (update :snake snake/add-food))

      world)))

(defn advance
  "Advances the world one tick.
  Returns nil if the snake collided with itself."
  [world rand-gen]
  (some-> world
          (handle-snake-movement)
          (handle-eating rand-gen)))

(defn format-world [world]
  (let [{[w h] :dimensions, snake :snake, food :food} world
        snake-set (set (:body snake))

        rows (for [y (range h)]
               (for [x (range w)
                     :let [pos [x y]]]
                 (cond
                   (snake-set pos) "#"
                   (= pos food) "!"
                   :else " ")))]

    (->> rows
         (map #(s/join " " %))
         (s/join "\n"))))