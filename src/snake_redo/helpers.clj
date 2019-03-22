(ns snake-redo.helpers
  (:require [helpers.general-helpers :as g])
  (:import (java.util Random)))

(defn signum [n]
  (cond
    (pos? n) 1
    (neg? n) -1
    :else 0))

(defn abs [n]
  (if (pos? n) n (- n)))

(defn vec-swap [v i j]
  (-> v
      (assoc i (v j))
      (assoc j (v i))))

(defn vec-last [v]
  (get v (dec (count v))))

(defn simple-wrap [n min-bound max-bound]
  (+ (mod (- n min-bound) (- max-bound min-bound))
     min-bound))

(defn wrap-position [position dimensions]
  (mapv #(simple-wrap % 0 %2) position dimensions))

(defn random-point-in [dimensions rand-gen]
  (mapv #(g/random-int 0 % rand-gen)
        dimensions))

(defn positions-surrounding
  ([position depth]
   (let [[cx cy] position]
     (for [y (range (- cy depth) (+ cy depth 1))
           x (range (- cx depth) (+ cx depth 1))
           :let [pos [x y]]
           :when (not= pos position)]
       pos)))

  ([position]
   (positions-surrounding position 1)))

(defn possible-moves
  ([depth]
   (for [y (range (- depth) (+ depth 1))
         x (range (- depth) (+ depth 1))
         :let [off [x y]]
         :when (and (not= off [0 0])
                    (some zero? [x y]))]
     off))

  ([]
   (possible-moves 1)))

(defn line-from [position offset length]
  (->> position
       (iterate #(mapv + % offset))
       (drop 1)
       (take length)))