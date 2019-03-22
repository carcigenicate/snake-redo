(ns thread-test
  (:import (java.util Date)))

(defn -main []
  (let [t (Thread. ^Runnable
            (fn []
              (while true
                (println (Date.))
                (Thread/sleep 500))))]
    (doto t
      (.setDaemon true)
      (.start))

    (Thread/sleep 5000)
    (println "Exiting...")))