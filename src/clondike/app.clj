(ns clondike.app
  (:require [clojure.core.async :refer (<!! close! thread)]
            [com.stuartsierra.component :as component]
            [clondike.game.logic :as l]
            [clondike.ui.cli :as ui]))

(declare redraw)

(defn handle-move [app move]
  (if (not (nil? move))
    (let [updated-game-state (l/apply-move (:game-state app) move)
          updated-app (assoc app
                        :game-state updated-game-state
                        :possible-moves (l/possible-moves updated-game-state))]
      (redraw updated-app)
      updated-app)
    app))

(defn redeal [app]
  (let [new-game-state (l/make-game-state (l/generate-shuffled-deck))
        updated-app (assoc app
                      :game-state new-game-state
                      :possible-moves (l/possible-moves new-game-state))]
    (redraw updated-app)
    updated-app))

(defn get-stock-waste-move [possible-moves]
  (first (filter #(and (= :stock (:from %1)) (= :waste (:to %1))) possible-moves)))

(defn redraw [app]
  (let [screen (:screen app)]
    (ui/draw-screen screen)
    (ui/draw-game-state screen (:game-state app) (:possible-moves app))))

(defn await-input [app]
  (let [command (<!! (:command-channel app))]
    (case command :quit (component/stop app)
                  :flip (await-input (handle-move app (get-stock-waste-move (:possible-moves app))))
                  :redeal (await-input (redeal app))
                  :toggle-select-current (await-input app)
                  :highlight-previous (await-input app)
                  :highlight-next (await-input app)
                  :redraw (do (redraw app) (await-input app))
                  (await-input app))))

(defrecord App []
  component/Lifecycle
  (start [app]
    (let [game-state (l/make-game-state (l/generate-shuffled-deck))
          app-with-state
          (assoc app
            :game-state game-state
            :possible-moves (l/possible-moves game-state)
            :highlighted-card nil
            :selected-card nil)]
      (redraw app-with-state)
      (thread (await-input app-with-state))
      app-with-state))
  (stop [app]
    (component/stop (:screen app))
    (close! (:command-channel app))
    (assoc app
      :game-state nil
      :possible-moves nil
      :highlighted-card nil
      :selected-card nil)))

