(ns dev.user
  (:require [com.stuartsierra.component :as component]
            [clojure.tools.namespace.repl :refer [refresh disable-reload!]]
            [sparkle.core :refer :all]
            [sparkle.layer :as l]))

(def system nil)

(defn init []
  (alter-var-root #'system
    (constantly (sparkle-system))))

(defn start []
  (alter-var-root #'system component/start))

(defn stop []
  (alter-var-root #'system
    (fn [s] (when s (component/stop s)))))

(defn go []
  (init)
  (start))

(defn reset []
  (stop)
  (refresh :after 'dev.user/go))

