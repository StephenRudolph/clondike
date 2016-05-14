(ns clondike.app
  (:require [clojure.core.async :refer (<!! >!! close! thread)]
            [com.stuartsierra.component :as component]
            [clondike.game.logic :as l]
            [clondike.ui.cli :as ui]
            [com.rpl.specter :as s]))

(declare redraw)

(def highlightable-paths
  [(s/comp-paths :game-state :waste s/FIRST)
   (s/comp-paths :game-state :tableau s/FIRST s/FIRST)
   (s/comp-paths :game-state :tableau (s/srange 1 2) s/FIRST s/FIRST)
   (s/comp-paths :game-state :tableau (s/srange 2 3) s/FIRST s/FIRST)
   (s/comp-paths :game-state :tableau (s/srange 3 4) s/FIRST s/FIRST)
   (s/comp-paths :game-state :tableau (s/srange 4 5) s/FIRST s/FIRST)
   (s/comp-paths :game-state :tableau (s/srange 5 6) s/FIRST s/FIRST)
   (s/comp-paths :game-state :tableau (s/srange 6 7) s/FIRST s/FIRST)
   (s/comp-paths :game-state :foundations :spades s/FIRST)
   (s/comp-paths :game-state :foundations :hearts s/FIRST)
   (s/comp-paths :game-state :foundations :clubs s/FIRST)
   (s/comp-paths :game-state :foundations :diamonds s/FIRST)])

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
    (ui/draw-game-state screen (:game-state app) (:possible-moves app) (:selected-card app)))
  app)

(defn transform-all-cards [app tranform-fn]
  (->> (s/transform [:game-state :stock s/ALL] tranform-fn app)
       (s/transform [:game-state :waste s/ALL] tranform-fn)
       (s/transform [:game-state :tableau s/ALL s/ALL] tranform-fn)
       (s/transform [:game-state :foundations :spades s/ALL] tranform-fn)
       (s/transform [:game-state :foundations :hearts s/ALL] tranform-fn)
       (s/transform [:game-state :foundations :clubs s/ALL] tranform-fn)
       (s/transform [:game-state :foundations :diamonds s/ALL] tranform-fn)))

(defn unhighlight-all [app]
  (transform-all-cards app #(dissoc % :highlighted)))

(defn unselect-all [app]
  (transform-all-cards app #(dissoc % :selected)))

(defn highlight-flipped [app]
  (let [waste (s/select [:game-state :waste] app)]
    (if (empty? waste)
      app
      (s/transform [:game-state :waste s/FIRST] #(assoc % :highlighted true) app))))

(defn toggle-select-on-highlighted [app]
  (let [highlighted-path (highlightable-paths (:highlighted-path-index app))
        highlighted-card (first (s/select highlighted-path app))]
    (if (contains? highlighted-card :selected)
      (assoc (s/transform highlighted-path #(dissoc % :selected) app) :selected-card nil)
      (assoc (s/transform highlighted-path #(assoc % :selected true) app) :selected-card highlighted-card))))

(defn reset-highlighted-path-index [app]
  (assoc app :highlighted-path-index 0))

(defn highlight-next [app]
  (let [incremented (s/transform [:highlighted-path-index] #(mod (inc %) (count highlightable-paths)) app)
        next-path (highlightable-paths (:highlighted-path-index incremented))
        next-card (s/select next-path incremented)]
    (if (nil? next-card)
      (highlight-next incremented)
      (s/transform next-path #(assoc % :highlighted true) incremented))))

(defn highlight-previous [app]
  (let [decremented (s/transform [:highlighted-path-index] #(mod (dec %) (count highlightable-paths)) app)
        next-path (highlightable-paths (:highlighted-path-index decremented))
        next-card (s/select next-path decremented)]
    (if (nil? next-card)
      (highlight-previous decremented)
      (s/transform next-path #(assoc % :highlighted true) decremented))))

(defn await-input [app]
  (loop [current-app app
         command (<!! (:command-channel app))]
    (case command :quit (component/stop current-app)
                  :flip (-> (handle-move current-app (get-stock-waste-move (:possible-moves current-app)))
                            (unhighlight-all)
                            (unselect-all)
                            (highlight-flipped)
                            (reset-highlighted-path-index)
                            (redraw)
                            (recur (<!! (:command-channel current-app))))
                  :redeal (-> (redeal current-app)
                              (recur (<!! (:command-channel current-app))))
                  :toggle-select-current (-> (toggle-select-on-highlighted current-app)
                                             (redraw)
                                             (recur (<!! (:command-channel current-app))))
                  :highlight-previous (-> (unhighlight-all current-app)
                                          (highlight-previous)
                                          (redraw)
                                          (recur (<!! (:command-channel current-app))))
                  :highlight-next (-> (unhighlight-all current-app)
                                      (highlight-next)
                                      (redraw)
                                      (recur (<!! (:command-channel current-app))))
                  :redraw (-> (redraw current-app)
                              (recur (<!! (:command-channel current-app))))
                  (recur current-app (<!! (:command-channel current-app))))))

(defrecord App [stop-chan]
  component/Lifecycle
  (start [app]
    (let [game-state (l/make-game-state (l/generate-shuffled-deck))
          app-with-state
          (assoc app
            :game-state (s/transform [:tableau s/FIRST s/FIRST] #(assoc % :highlighted true) game-state)
            :possible-moves (l/possible-moves game-state)
            :selected-card nil
            :highlighted-path-index 1)]
      (redraw app-with-state)
      (thread (await-input app-with-state))
      app-with-state))
  (stop [app]
    (component/stop (:screen app))
    (close! (:command-channel app))
    (>!! stop-chan :stop)
    (assoc app
      :game-state nil
      :possible-moves nil
      :selected-card nil
      :highlighted-path-index nil)))