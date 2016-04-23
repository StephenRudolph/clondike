(ns clondike.core
  (:gen-class)
  (:require [clondike.ui.cli :as cli]))

(defn -main
  "Start a new game of Clondike"
  [& args]
  (cli/launch!))