(ns snake-redo.ui.helpers
  (:require [seesaw.core :as sc]
            [seesaw.graphics :as sg])
  (:import [java.awt Component FontMetrics Graphics Font]
           (javax.swing Timer JPanel)
           (java.util.concurrent Executors ThreadFactory ExecutorService)))

(defn get-root [^Component c]
  (println "At" c)
  (if-let [parent (.getParent c)]
    (recur parent)
    c))

(defn component-dimensions [^Component c]
  [(.getWidth c) (.getHeight c)])

(defn start-stoppable-timer [f update-delay]
  (let [t (sc/timer (fn [_] (f))
                    :initial-delay update-delay,
                    :delay update-delay)]

    (fn [] (.stop ^Timer t))))

(defn draw-centered-text [^JPanel canvas, ^Graphics g, ^Font font, ^String text]
    (.setFont g font)

    (let [font-metr (.getFontMetrics g)
          txt-width (.stringWidth ^FontMetrics font-metr, text)
          txt-height (.getHeight font-metr)

          [cw ch] (component-dimensions canvas)]

      (sg/draw g
         (sg/string-shape (long (- (* 0.5 cw) (* 0.5 txt-width)))
                          (long (- (* 0.5 ch) (* 0.5 txt-height)))
                          text)
         (sg/style :foreground :black))))

(defn start-stoppable-timer [action delay]
  (let [t (sc/timer (fn [_] (action))
                    :initial-delay delay, :delay delay)]

    (fn []
      (.stop ^Timer t))))

(defn new-daemon-pool ^ExecutorService [n-threads]
  (Executors/newFixedThreadPool
    n-threads
    (reify ThreadFactory
      (^Thread
       newThread [this, ^Runnable r]
        (doto (.newThread (Executors/defaultThreadFactory) r)
          (.setDaemon true))))))

