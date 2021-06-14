(ns battlesnake.core
  (:require [battlesnake.server :as server])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (server/run))
