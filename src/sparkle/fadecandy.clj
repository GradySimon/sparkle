(ns sparkle.fadecandy
  (:import (java.net Socket))
  (:require [clojure.algo.generic.functor :refer [fmap]]
            [com.evocomputing.colors :as c :refer [create-color] :rename {create-color color}]
            [sparkle.util :refer [constrain]]))

(defn open-connection [host port]
  (let [socket (java.net.Socket. host port)]
    (.setTcpNoDelay socket true)
    (.getOutputStream socket)))

(defn close-connection [connection]
  (.close connection))

(defn prepare-pixel [pixel]
  ((juxt c/red c/green c/blue) pixel))

(defn push-pixels
  "Pushes pixels in pixel-map to the device over connection.
   a map of channel numbers to vectors of pixels, packages
   them into a set-color packet, and pushes them to the FadeCandy
   device."
  [pixel-map connection]
   (doseq [[channel-num pixels] pixel-map]
     (let [command-num 0 ; set color command per OPC
           pixel-byte-length (* 3 (count pixels))
           pixel-byte-length-high (quot pixel-byte-length 256)
           pixel-byte-length-low (mod pixel-byte-length 256)
           header [channel-num command-num pixel-byte-length-high pixel-byte-length-low]]
       (->> pixels
           (mapcat prepare-pixel)
           (concat header)
           (byte-array)
           (.write connection))
        (.flush connection))))

