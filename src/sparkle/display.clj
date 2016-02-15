(ns sparkle.display
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [com.stuartsierra.component :as component]
            [sparkle.util :refer [now]]
            [sparkle.fadecandy :as fc]))

(defprotocol Displayer
  "A protocol for things that are capable of displaying frames"
  (display [displayer frame] "Display a frame"))

;;; Displaying to console

;; The minimum nowtime, in millis, between frames printed by ConsoleDisplayer
(def console-display-interval 500)

(defrecord ConsoleDisplayer [previous]
  component/Lifecycle
  (start [displayer]
    (assoc displayer
           :previous (atom {:frame nil
                            :time (- (now) console-display-interval)})))

  (stop [displayer]
    (dissoc displayer :previous))

  Displayer
  (display [_ frame]
    (let [{prev-frame :frame prev-time :time} @previous]
      (when (and (>= (now) (+ prev-time console-display-interval))
                 (not= frame prev-frame))
        (reset! previous {:frame frame :time (now)})
        (println frame)))))

;;; Displaying to Fadecandy

(defrecord FadecandyDisplayer [host port connection]
  component/Lifecycle
  (start [displayer]
    (assoc displayer :connection (fc/open-connection host port)))

  (stop [displayer]
    (fc/close-connection connection)
    (dissoc displayer :connection))

  Displayer
  (display [_ frame]
    (fc/push-pixels {0 frame} connection)))

(defn new-fadecandy-displayer [host port]
  (map->FadecandyDisplayer {:host host :port port}))
