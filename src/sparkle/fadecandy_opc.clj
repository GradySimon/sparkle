(ns sparkle.fadecandy-opc
  (:import (java.net Socket))
  (:require [gloss.core :as gloss]
            [gloss.io :as gloss-io]))

(def server-address
  {:host "localhost"
   :port 7890})

(defn open-fc-conn [{:keys [host port]}]
  (let [socket (java.net.Socket. host port)]
    (.setTcpNoDelay socket true)
    (.getOutputStream socket)))

(defonce fc-conn (atom (open-fc-conn server-address)))

(defn send-packet [packet]
  (.write @fc-conn packet))

(defn color-bytes [pixel]
  (map #(byte (min 255 %)) [(:r pixel) (:g pixel) (:b pixel)]))

(gloss/defcodec color
  (gloss/ordered-map
    :r :ubyte
    :g :ubyte
    :b :ubyte))

(gloss/defcodec set-color-packet
  (gloss/ordered-map
    :channel-num :ubyte
    :command :ubyte
    :pixels (gloss/finite-frame :int16-be
              (gloss/repeated color :prefix :none))))

(defn push-pixels [pixel-map]
  "Takes a map of channel numbers to vectors of pixels, packages
   them into a set-color packet, and pushes them to the FadeCandy
   device."
  (doseq [[channel-num pixels] pixel-map]
    (let [pixel-byte-count (* 3 (count pixels))]
      (println pixels)
      (gloss-io/encode-to-stream set-color-packet @fc-conn
        (seq
          [{:channel-num channel-num
            :command 0
            :pixels pixels}])))))

