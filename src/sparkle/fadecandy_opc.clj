(ns sparkle.fadecandy-opc
  (:import (java.net Socket))
  (:require [clojure.core.async :as a
             :refer [>! <! >!! <!! go go-loop chan buffer close! thread
                     alt! alts! alts!! timeout]]
            [gloss.core :as gloss]
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
    :length :int16-be
    :pixels (gloss/repeated color :prefix :none)))

(defn color-clip [value]
  (min 255 (max 0 value)))

(defn pixel->seq [{:keys [r g b]}]
  (map color-clip [r g b]))

(defn push-pixels [pixel-map]
  "Takes a map of channel numbers to vectors of pixels, packages
   them into a set-color packet, and pushes them to the FadeCandy
   device."
   (doseq [[channel-num pixels] pixel-map]
     (let [command-num 0 ; set color command per OPC
           pixel-byte-length (* 3 (count pixels))
           pixel-byte-length-high (quot pixel-byte-length 256)
           pixel-byte-length-low (mod pixel-byte-length 256)
           header [channel-num command-num pixel-byte-length-high pixel-byte-length-low]
           pixel-bytes (mapcat pixel->seq pixels)
           set-color-packet (byte-array (concat header pixel-bytes))]
        (.write @fc-conn set-color-packet)
        (.flush @fc-conn))))

  ; (doseq [[channel-num pixels] pixel-map]
  ;   (let [pixel-byte-count (* 3 (count pixels))]
  ;     (gloss-io/encode-to-stream set-color-packet @fc-conn
  ;       (seq
  ;         [{:channel-num channel-num
  ;           :command 0
  ;           :length pixel-byte-count
  ;           :pixels pixels}]))
  ;     (.flush @fc-conn)
  ;     (println (seq (.array (gloss-io/contiguous
  ;                       (gloss-io/encode set-color-packet
  ;                         {:channel-num channel-num
  ;                          :command 0
  ;                          :length pixel-byte-count
  ;                          :pixels pixels}))))))))

(defn start-pushing-pixels [pixel-chan]
  (go-loop []
    (let [pixels (<! pixel-chan)]
      (push-pixels pixels)
      (recur))))
