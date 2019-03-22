(ns snake-redo.logic.ai
  (:require [snake-redo.logic.snake :as snake]
            [snake-redo.helpers :as snh]

            [helpers.general-helpers :as g]))

(def legal-moves (snh/possible-moves))

(defn dumb-direction [world]
  (let [{:keys [snake food]} world
        [hx hy] (snake/get-head snake)
        [fx fy] food]
    (->> (if (not= fx hx)
           [(- fx hx) 0]
           [0 (- fy hy)])

         (mapv snh/signum))))

; TODO: Could be more efficient. `set` here isn't ideal.
(defn random-direction [world rand-gen]
  (let [{:keys [snake]} world
        snake-set (set (:body snake))
        head (snake/get-head snake)

        dumb-move (dumb-direction world)]

    (if (snake-set (mapv + head dumb-move))
      (let [valid-moves (->> legal-moves
                             (remove #(snake-set (mapv + head %)))
                             (vec))]

        (or (g/random-from-collection valid-moves rand-gen)
            dumb-move)) ; No winning moves, but we need to pick something.

      dumb-move)))

(defn dumb-direction-scanner [world depth]
  (let [{:keys [snake]} world
        snake-set (set (:body snake))
        head (snake/get-head snake)

        dumb-move (dumb-direction world)]

    (if (snake-set (mapv + head dumb-move))
      (let [next-sq (->> legal-moves
                         (map #(snh/line-from head % depth))
                         (map #(vector (first %)
                                       (count (remove snake-set %))))
                         ((fn [r] (println (count snake-set) (mapv second r)) r))
                         (sort-by second >)
                         (ffirst))]

        (->> (mapv - next-sq head)
             (mapv snh/signum)))

      dumb-move)))

(defn continuous-free-cells [snake-pred line]
  (->> line
       (take-while (complement snake-pred))
       (count)))

; TODO: An AI that checks for clear stetches instead of just total free in the range
(defn straight-direction-scanner [world depth]
  (let [{:keys [snake]} world
        snake-set (set (:body snake))
        head (snake/get-head snake)

        dumb-move (dumb-direction world)]

    (if (snake-set (mapv + head dumb-move))
      (let [next-sq (->> legal-moves
                         (mapv #(snh/line-from head % depth))
                         (mapv #(vector (first %)
                                        (continuous-free-cells snake-set %)))
                         ; ((fn [r] (println (count snake-set) head r) r))
                         (sort-by second >)
                         (ffirst))]

        (->> (mapv - next-sq head) ; Turn the location into an offset
             (mapv snh/signum)))

      dumb-move)))
