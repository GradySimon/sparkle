(ns sparkle.fadecandy-opc
  (:import (java.net Socket))
  (:require [org.clojars.smee.binary.core :as b]))

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

(def color
  (b/ordered-map
    :r :ubyte
    :g :ubyte
    :b :ubyte))

(def set-color-packet
  (b/ordered-map
    :channel-num :ubyte
    :command (b/constant :ubyte 0)
    :length :short-be
    :pixels (b/repeated color)))

(def put-pixels [pixels]
  )

