(ns clondike.ui.cli)

(require '[clondike.game.logic :as l])
(require '[lanterna.screen :as s])

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

(def screen-size (ref [0 0]))

(declare await-input)
(declare draw-screen)
(declare quit!)
(declare screen)

(def game-state (atom nil))
(def possible-moves (atom '()))
(def selected-card (atom nil))
(def selected-card-position (atom nil))
(def card-positions (atom {:spades [37 1] :hearts [44 1] :clubs [51 1] :diamonds [58 1]}))

(defn handle-resize [cols rows]
  (dosync (ref-set screen-size [cols rows]))
  (s/clear screen)
  (draw-screen)
  (s/redraw screen))

(def screen (s/get-screen :auto {:cols 80 :rows 24 :resize-listener handle-resize}))

(defn draw-box [left top width height style]
  (let [bottom (+ top (dec height))
        right (+ left (dec width))
        middle-width (- width 2)
        middle-height (- height 2)]
    (s/put-string screen left top (apply str top-left-corner (repeat middle-width horizontal-edge)) style)
    (s/put-string screen right top (str top-right-corner) style)
    (dotimes [i middle-height]
      (s/put-string screen left (+ i top 1) (str vertical-edge) style)
      (s/put-string screen right (+ i top 1) (str vertical-edge) style))
    (s/put-string screen left bottom (apply str bottom-left-corner (repeat middle-width horizontal-edge)) style)
    (s/put-string screen right bottom (str bottom-right-corner) style)))

(defn draw-right-semi-box [left top width height style]
  (let [bottom (+ top (dec height))
        right (+ left (dec width))
        middle-width (- width 1)
        middle-height (- height 2)]
    (s/put-string screen left top (apply str (repeat middle-width horizontal-edge)) style)
    (s/put-string screen right top (str top-right-corner) style)
    (dotimes [i middle-height]
      (s/put-string screen right (+ i top 1) (str vertical-edge) style))
    (s/put-string screen left bottom (apply str (repeat middle-width horizontal-edge)) style)
    (s/put-string screen right bottom (str bottom-right-corner) style)))

(defn draw-border []
  (draw-box 0 0 (screen-size 0) (screen-size 1) {}))

(defn draw-commands []
  (s/put-string screen 1 (dec (screen-size 1)) " F: Flip " {:fg :black :bg :white})
  (s/put-string screen (- (screen-size 0) 23) (dec (screen-size 1)) " R: Redeal " {:fg :black :bg :white})
  (s/put-string screen (- (screen-size 0) 10) (dec (screen-size 1)) " Q: Quit " {:fg :black :bg :white}))

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

(defn highlight-potential-move-source [card]
  (if (empty? (filter #(= card (:card %1)) @possible-moves))
    {}
    (if (contains? l/red-suits (:suit card))
      {:fg :black :bg :red}
      {:fg :black :bg :blue})))

(defn get-style [card]
  (if (nil? card)
    {:fg :white}
    (if (not (:flipped card))
      {:fg :green}
      (merge (get-suit-style (:suit card))
             (highlight-potential-move-source card)))))

(defn draw-card
  ([card pos]
   (let [[x y] pos]
     (draw-card card x y)))
  ([card x y]
   (let [style (get-style card)
         width 5
         height 5
         middle-horizontal-index (+ x 2)
         middle-vertical-index (+ y 2)]
     (draw-box x y width height style)
     (if (nil? card)
       (s/put-string screen middle-horizontal-index middle-vertical-index (str no-card))
       (if (:flipped card)
         (let [suit-label (str ((:suit card) suit-labels))
               card-label (str (get-card-label card))]
           (s/put-string screen (inc x) (inc y) suit-label)
           (if (= (:value card) 10)
             (s/put-string screen (dec middle-horizontal-index) middle-vertical-index card-label)
             (s/put-string screen middle-horizontal-index middle-vertical-index card-label))
           (s/put-string screen (inc middle-horizontal-index) (inc middle-vertical-index) suit-label))
         (dotimes [i (- width 2)]
           (dotimes [j (- height 2)]
             (s/put-string screen (+ i (inc x)) (+ j (inc y)) (str card-back) style))))))
   (+ x 5)))

(defn draw-card-right-edge [card x y]
  (let [style (get-style card)]
    (s/put-string screen x y (str top-right-corner) style)
    (dotimes [i 3]
      (s/put-string screen x (+ y i 1) (str vertical-edge) style))
    (s/put-string screen x (+ y 4) (str bottom-right-corner) style))
  (inc x))

(defn draw-card-partly-overlapped-on-left [card x y]
  (let [style (get-style card)
        width 3
        height 5
        middle-vertical-index (+ y 2)]
    (draw-right-semi-box x y width height style)
    (if (nil? card)
      (s/put-string screen x middle-vertical-index (str no-card))
      (if (:flipped card)
        (let [suit-label (str ((:suit card) suit-labels))
              card-label (str (get-card-label card))]
          (if (= (:value card) 10)
            (s/put-string screen x middle-vertical-index "0")
            (s/put-string screen x middle-vertical-index card-label))
          (s/put-string screen (inc x) (inc middle-vertical-index) suit-label))
        (dotimes [i (- width 2)]
          (dotimes [j (- height 2)]
            (s/put-string screen (+ i (inc x)) (+ j (inc y)) (str card-back) style))))))
  (+ x 3))

(defn draw-card-top-edge [card x y]
  (let [style (get-style card)]
    (s/put-string screen x y (str top-left-corner) style)
    (dotimes [i 3]
      (s/put-string screen (+ x i 1) y (str horizontal-edge) style))
    (s/put-string screen (+ x 4) y (str top-right-corner) style))
  (+ x 5))

(defn draw-board []
  )

(defn draw-stock [x stock]
  (if (empty? stock)
    0
    (if (> x 2)
      (let [top 1]
        (dotimes [i (count stock)]
          (draw-card-right-edge {:flipped false} (+ i x) top))
        (count stock))
      (let [top 1
            extra-left (draw-card (peek stock) x top)]
        (dotimes [i (dec (count stock))]
          (draw-card-right-edge {:flipped false} (+ i extra-left) top))
        (+ extra-left (dec (count stock)))))))

(defn draw-waste-recur [x y shown-waste hidden-waste]
  (if (empty? shown-waste)
    (if (empty? hidden-waste)
      nil
      (let [xIndent (draw-card-right-edge (peek hidden-waste) x y)]
        (draw-waste-recur xIndent
                          y
                          shown-waste
                          (pop hidden-waste))))
    (let [xIndent (draw-card-partly-overlapped-on-left (peek shown-waste) x y)]
      (draw-waste-recur xIndent
                        y
                        (pop shown-waste)
                        hidden-waste))))

(defn draw-waste [waste]
  (let [[shown-waste hidden-waste] (l/queue-split-at 3 waste)
        top 1
        left (if (empty? shown-waste)
               2
               (do
                 (reset! card-positions (assoc @card-positions :waste [2 top]))
                 (draw-card (peek shown-waste) 2 top)))]
    (draw-waste-recur left top (pop shown-waste) hidden-waste)
    (+ left (* 3 (count (pop shown-waste))) (count hidden-waste))))

(defn draw-foundations [foundations]
  (draw-card (peek (:spades foundations)) (:spades @card-positions))
  (draw-card (peek (:hearts foundations)) (:hearts @card-positions))
  (draw-card (peek (:clubs foundations)) (:clubs @card-positions))
  (draw-card (peek (:diamonds foundations)) (:diamonds @card-positions)))

(defn draw-tableau-recur [tableau current-index]
  (if (>= current-index (count tableau))
    nil                                                     ; Done
    (let [cards (reverse (get tableau current-index))
          left (+ (* current-index 7) 2)
          last-top (+ (count cards) 5)]
      (dotimes [i (dec (count cards))]
        (draw-card-top-edge (nth cards i) left (+ i 6)))
      (if (not (empty? cards))
        (do
          (reset! card-positions (assoc @card-positions current-index [left last-top]))
          (draw-card (last cards) left last-top)))
      (draw-tableau-recur tableau (inc current-index)))))

(defn draw-tableau [tableau]
  (draw-tableau-recur tableau 0))

(defn draw-screen []
  (s/clear screen)
  (draw-border)
  (draw-commands)
  (draw-board)
  (let [x (draw-waste (:waste @game-state))]
    (draw-stock x (:stock @game-state)))
  (draw-foundations (:foundations @game-state))
  (draw-tableau (:tableau @game-state)))

(defn handle-move [move]
  (if (not (nil? move))
    (do
      (reset! game-state (l/apply-move @game-state move))
      (reset! possible-moves (l/possible-moves @game-state))
      (draw-screen)
      (s/redraw screen)
      (await-input))))

(defn redeal! []
  (reset! game-state (l/make-game-state (l/generate-shuffled-deck)))
  (reset! possible-moves (l/possible-moves @game-state))
  (draw-screen)
  (s/redraw screen)
  (await-input))

(defn get-stock-waste-move []
  (first (filter #(and (= :stock (:from %1)) (= :waste (:to %1))) @possible-moves)))

(defn await-input []
  (let [key (s/get-key-blocking screen)]
    (case key \q (quit!)
              \Q (quit!)
              \f (handle-move (get-stock-waste-move))
              \F (handle-move (get-stock-waste-move))
              \r (redeal!)
              \R (redeal!)
              (await-input))))

(defn launch! []
  (s/start screen)
  (let [size (s/get-size screen)]
    (handle-resize (size 0) (size 1)))
  (redeal!)
  #_(s/move-cursor screen 2 (dec (screen-size 1))))

(defn quit! []
  (s/stop screen))