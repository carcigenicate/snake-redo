(ns snake-redo.logic.snake
  (:require [snake-redo.helpers :as snh]))

(def starting-tail-offset [0 1])

; ---------- Body ----------

(defn new-snake [starting-position])


(defn new-snake
  ([starting-position starting-direction]
   {:body [starting-position]
    :direction starting-direction})

  ([starting-position]
   (new-snake starting-position [0 0])))

(defn get-head [snake]
  (-> snake
      (:body)
      (snh/vec-last)))

(defn get-tail-tip [snake]
  (-> snake
      (:body)
      (first)))

(defn length [snake]
  (-> snake
      (:body)
      (count)))

(defn get-tail [snake]
  (-> snake
      (:body)
      (subvec 0 (dec (length snake)))))

(defn- move-to [snake new-position]
  (update snake :body #(-> %
                           (conj new-position)
                           (subvec 1))))

(defn move-by [snake offsets]
  (move-to snake
           (mapv + (get-head snake) offsets)))

(defn head-overlapping-neck? [snake]
  (let [len (length snake)]
    (and (>= len 2)
         (= (get-head snake)
            (get snake (- len 2))))))

(defn head-overlapping-body? [snake]
  (let [head (get-head snake)]
    (->> snake
         (get-tail)
         (some #(= % head)))))

(defn- new-tail-end-position [snake]
  (let [len (length snake)
        {:keys [body direction]} snake

        offset (if (> len 1)
                 (let [tail-end-pair (subvec body 0 2)]
                   (apply mapv - tail-end-pair))

                 (mapv #(* -1 %) direction))]

    (mapv + (get-tail-tip snake) offset)))

(defn add-food [snake]
  (let [new-pos (new-tail-end-position snake)]
    (update snake :body
            #(into [new-pos] %))))

(defn wrap-snake-ends [snake dimensions]
  (update snake :body
          (fn [b]
            (-> b
                (update 0 #(snh/wrap-position % dimensions))
                (update (dec (count b)) #(snh/wrap-position % dimensions))))))

(defn change-direction [snake new-direction]
  (assoc snake :direction new-direction))

(defn move-in-direction [snake]
  (move-by snake (:direction snake)))

(defn checked-changed-directions
  "Ensures the new direction won't cause the snake to collide with its neck.
  Returns the snake with a new direction, or nil if the change failed."
  [snake new-direction]
  (let [changed (change-direction snake new-direction)
        advanced (move-in-direction changed)]
    (when-not (head-overlapping-neck? advanced)
      changed)))