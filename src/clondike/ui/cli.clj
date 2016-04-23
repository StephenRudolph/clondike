(ns clondike.ui.cli
  (:require [com.stuartsierra.component :as component])
  (:require [lanterna.screen :as s])
  (:require [clondike.game.logic :as l]))

(def ^:const suit-labels
  {:spades \u2660 :hearts \u2665 :clubs \u2663 :diamonds \u2666})

(def ^:const top-left-corner \u250C)
(def ^:const top-right-corner \u2510)
(def ^:const bottom-left-corner \u2514)
(def ^:const bottom-right-corner \u2518)
(def ^:const horizontal-edge \u2500)
(def ^:const vertical-edge \u2502)

(def ^:const no-card \O)
(def ^:const card-back \X)

#_(def screen-size (ref [0 0]))

(declare await-input)
(declare draw-screen)
(declare quit!)
(declare screen)

(def possible-moves (atom '()))
(def selected-card (atom nil))
(def selected-card-position (atom nil))

#_(defn handle-resize [cols rows]
  (dosync (ref-set screen-size [cols rows]))
  (s/clear screen)
  (draw-screen)
  (s/redraw screen))

#_(def screen (s/get-screen :auto {:cols 80 :rows 24 :resize-listener handle-resize}))

#_(let [size (s/get-size screen)]
    (handle-resize (size 0) (size 1)))

#_(s/move-cursor screen 2 (dec (screen-size 1)))

(defn draw-box [app left top width height style]
  (let [bottom (+ top (dec height))
        right (+ left (dec width))
        middle-width (- width 2)
        middle-height (- height 2)]
    (s/put-string (:screen app) left top (apply str top-left-corner (repeat middle-width horizontal-edge)) style)
    (s/put-string (:screen app) right top (str top-right-corner) style)
    (dotimes [i middle-height]
      (s/put-string (:screen app) left (+ i top 1) (str vertical-edge) style)
      (s/put-string (:screen app) right (+ i top 1) (str vertical-edge) style))
    (s/put-string (:screen app) left bottom (apply str bottom-left-corner (repeat middle-width horizontal-edge)) style)
    (s/put-string (:screen app) right bottom (str bottom-right-corner) style)))

(defn draw-right-semi-box [app left top width height style]
  (let [bottom (+ top (dec height))
        right (+ left (dec width))
        middle-width (- width 1)
        middle-height (- height 2)]
    (s/put-string (:screen app) left top (apply str (repeat middle-width horizontal-edge)) style)
    (s/put-string (:screen app) right top (str top-right-corner) style)
    (dotimes [i middle-height]
      (s/put-string (:screen app) right (+ i top 1) (str vertical-edge) style))
    (s/put-string (:screen app) left bottom (apply str (repeat middle-width horizontal-edge)) style)
    (s/put-string (:screen app) right bottom (str bottom-right-corner) style)))

(defn draw-border [app]
  (draw-box app 0 0 ((:screen-size app) 0) ((:screen-size app) 1) {}))

(defn draw-commands [app]
  (s/put-string screen 1 (dec ((:screen-size app) 1)) " F: Flip " {:fg :black :bg :white})
  (s/put-string screen (- ((:screen-size app) 0) 23) (dec ((:screen-size app) 1)) " R: Redeal " {:fg :black :bg :white})
  (s/put-string screen (- ((:screen-size app) 0) 10) (dec ((:screen-size app) 1)) " Q: Quit " {:fg :black :bg :white}))

(defn get-card-label [card]
  (let [n (:value card)]
    (case n 1 \A
            11 \J
            12 \Q
            13 \K
            n)))

(defn get-suit-style [suit]
  (if (contains? l/red-suits suit)
    {:fg :red}
    {:fg :blue}))

(defn highlight-potential-move-source [app card]
  (if (empty? (filter #(= card (:card %1)) (:possible-moves app)))
    {}
    (if (contains? l/red-suits (:suit card))
      {:fg :black :bg :red}
      {:fg :black :bg :blue})))

(defn get-style [app card]
  (if (nil? card)
    {:fg :white}
    (if (not (:flipped card))
      {:fg :green}
      (merge (get-suit-style (:suit card))
             (highlight-potential-move-source app card)))))

(defn draw-card
  ([app card pos]
   (let [[x y] pos]
     (draw-card app card x y)))
  ([app card x y]
   (let [style (get-style app card)
         width 5
         height 5
         middle-horizontal-index (+ x 2)
         middle-vertical-index (+ y 2)]
     (draw-box app x y width height style)
     (if (nil? card)
       (s/put-string (:screen app) middle-horizontal-index middle-vertical-index (str no-card))
       (if (:flipped card)
         (let [suit-label (str ((:suit card) suit-labels))
               card-label (str (get-card-label card))]
           (s/put-string (:screen app) (inc x) (inc y) suit-label)
           (if (= (:value card) 10)
             (s/put-string (:screen app) (dec middle-horizontal-index) middle-vertical-index card-label)
             (s/put-string (:screen app) middle-horizontal-index middle-vertical-index card-label))
           (s/put-string (:screen app) (inc middle-horizontal-index) (inc middle-vertical-index) suit-label))
         (dotimes [i (- width 2)]
           (dotimes [j (- height 2)]
             (s/put-string (:screen app) (+ i (inc x)) (+ j (inc y)) (str card-back) style))))))
   (+ x 5)))

(defn draw-card-right-edge [app card x y]
  (let [style (get-style app card)]
    (s/put-string (:screen app) x y (str top-right-corner) style)
    (dotimes [i 3]
      (s/put-string (:screen app) x (+ y i 1) (str vertical-edge) style))
    (s/put-string (:screen app) x (+ y 4) (str bottom-right-corner) style))
  (inc x))

(defn draw-card-partly-overlapped-on-left [app card x y]
  (let [style (get-style app card)
        width 3
        height 5
        middle-vertical-index (+ y 2)]
    (draw-right-semi-box app x y width height style)
    (if (nil? card)
      (s/put-string (:screen app) x middle-vertical-index (str no-card))
      (if (:flipped card)
        (let [suit-label (str ((:suit card) suit-labels))
              card-label (str (get-card-label card))]
          (if (= (:value card) 10)
            (s/put-string (:screen app) x middle-vertical-index "0")
            (s/put-string (:screen app) x middle-vertical-index card-label))
          (s/put-string (:screen app) (inc x) (inc middle-vertical-index) suit-label))
        (dotimes [i (- width 2)]
          (dotimes [j (- height 2)]
            (s/put-string (:screen app) (+ i (inc x)) (+ j (inc y)) (str card-back) style))))))
  (+ x 3))

(defn draw-card-top-edge [app card x y]
  (let [style (get-style app card)]
    (s/put-string (:screen app) x y (str top-left-corner) style)
    (dotimes [i 3]
      (s/put-string (:screen app) (+ x i 1) y (str horizontal-edge) style))
    (s/put-string (:screen app) (+ x 4) y (str top-right-corner) style))
  (+ x 5))

(defn draw-board [app]
  )

(defn draw-stock [app x]
  (let [stock (:stock (:game-state app))]
  (if (empty? stock)
    0
    (if (> x 2)
      (let [top 1]
        (dotimes [i (count stock)]
          (draw-card-right-edge app {:flipped false} (+ i x) top))
        (count stock))
      (let [top 1
            extra-left (draw-card app (peek stock) x top)]
        (dotimes [i (dec (count stock))]
          (draw-card-right-edge app {:flipped false} (+ i extra-left) top))
        (+ extra-left (dec (count stock))))))))

(defn draw-waste-recur [app x y shown-waste hidden-waste]
  (if (empty? shown-waste)
    (if (empty? hidden-waste)
      nil
      (let [xIndent (draw-card-right-edge app (peek hidden-waste) x y)]
        (draw-waste-recur app
                          xIndent
                          y
                          shown-waste
                          (pop hidden-waste))))
    (let [xIndent (draw-card-partly-overlapped-on-left app (peek shown-waste) x y)]
      (draw-waste-recur app
                        xIndent
                        y
                        (pop shown-waste)
                        hidden-waste))))

(defn draw-waste [app]
  (let [[shown-waste hidden-waste] (l/queue-split-at 3 (:waste (:game-state app)))
        top 1
        left (if (empty? shown-waste)
               2
               (do
                 (reset! card-positions (assoc @card-positions :waste [2 top]))
                 (draw-card app (peek shown-waste) 2 top)))]
    (draw-waste-recur app left top (pop shown-waste) hidden-waste)
    (+ left (* 3 (count (pop shown-waste))) (count hidden-waste))))

(defn draw-foundations [app]
  (let [foundations (:foundations (:game-state app))]
  (draw-card app (peek (:spades foundations)) (:spades (:card-positions app)))
  (draw-card app (peek (:hearts foundations)) (:hearts (:card-positions app)))
  (draw-card app (peek (:clubs foundations)) (:clubs (:card-positions app)))
  (draw-card app (peek (:diamonds foundations)) (:diamonds (:card-positions app)))))

(defn draw-tableau-recur [app tableau current-index]
  (if (>= current-index (count tableau))
    nil                                                     ; Done
    (let [cards (reverse (get tableau current-index))
          left (+ (* current-index 7) 2)
          last-top (+ (count cards) 5)]
      (dotimes [i (dec (count cards))]
        (draw-card-top-edge app (nth cards i) left (+ i 6)))
      (if (not (empty? cards))
        (do
          (reset! card-positions (assoc @card-positions current-index [left last-top]))
          (draw-card app (last cards) left last-top)))
      (draw-tableau-recur app tableau (inc current-index)))))

(defn draw-tableau [app]
  (draw-tableau-recur app (:tableau game-state) 0))

(defn draw-screen [app]
  (s/clear screen)
  (draw-border app)
  (draw-commands app)
  (draw-board app)
  (let [x (draw-waste app)]
    (draw-stock app x))
  (draw-foundations app)
  (draw-tableau app))

(defn handle-move [app move]
  (if (not (nil? move))
    (do
      (reset! game-state (l/apply-move @game-state move))
      (reset! possible-moves (l/possible-moves @game-state))
      (draw-screen app)
      (s/redraw screen)
      (await-input app))))

(defn redeal! [app]
  (reset! game-state (l/make-game-state (l/generate-shuffled-deck)))
  (reset! possible-moves (l/possible-moves @game-state))
  (draw-screen app)
  (s/redraw screen)
  (await-input app))

(defn get-stock-waste-move [app]
  (first (filter #(and (= :stock (:from %1)) (= :waste (:to %1))) (:possible-moves app))))

(defn await-input [app]
  (let [key (s/get-key-blocking (:screen app))]
    (case key \q (quit!)
              \Q (quit!)
              \f (handle-move app (get-stock-waste-move app))
              \F (handle-move app (get-stock-waste-move app))
              \r (redeal!)
              \R (redeal!)
              (await-input app))))

(defrecord App []
  component/Lifecycle
  (start [app]
    (let [game-state (l/make-game-state (l/generate-shuffled-deck))
          screen (s/get-screen :auto {:cols 80 :rows 24})
          app-with-state
            (assoc app
              :game-state game-state
              :possible-moves (l/possible-moves game-state)
              :screen screen
              :screen-size [0 0]
              :card-positions {:spades [37 1] :hearts [44 1] :clubs [51 1] :diamonds [58 1]})]
        (s/start app-with-state)
        (draw-screen app-with-state)
        (s/redraw app-with-state)
        (await-input app-with-state)))
  (stop [app]
    (s/stop (:screen app))))