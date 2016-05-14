(ns clondike.ui.cli
  (:require [clojure.core.async :refer (>!! thread)]
            [com.stuartsierra.component :as component]
            [lanterna.screen :as s]
            [clondike.game.logic :as l]))

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

(declare await-input)

(defn draw-box [screen left top width height style]
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

(defn draw-right-semi-box [screen left top width height style]
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

(defn draw-border [cli]
  (let [screen (:screen cli)
        screen-size (s/get-size screen)]
    (draw-box screen 0 0 (screen-size 0) (screen-size 1) {})))

(defn draw-commands [cli]
  (let [screen (:screen cli)
        screen-size (s/get-size screen)]
    (s/put-string screen 1 (dec (screen-size 1)) " F: Flip " {:fg :black :bg :white})
    (s/put-string screen (- (screen-size 0) 23) (dec (screen-size 1)) " R: Redeal " {:fg :black :bg :white})
    (s/put-string screen (- (screen-size 0) 10) (dec (screen-size 1)) " Q: Quit " {:fg :black :bg :white})))

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

(defn show-potential-move-source [possible-moves card]
  (if (empty? (filter #(= card (:from-card %1)) possible-moves))
    {}
    (if (contains? l/red-suits (:suit card))
      {:fg :magenta}
      {:fg :cyan})))

(defn get-highlighted-style [card]
  (if (contains? card :highlighted)
    {:fg :yellow}
    {}))

(defn get-selected-style [card]
  (if (contains? card :selected)
    {:bg :yellow}
    {}))

(defn get-card-border-style [possible-moves selected-card card]
  (if (nil? card)
    {:fg :white}
    (if (not (:flipped card))
      {:fg :green}
      (merge (get-suit-style (:suit card))
             #_(show-potential-move-source possible-moves card)
             #_(show-potential-move-destination possible-moves selected-card card)
             (get-highlighted-style card)
             (get-selected-style card)))))

(defn draw-card
  ([screen possible-moves selected-card card pos]
   (let [[x y] pos]
     (draw-card screen possible-moves selected-card card x y)))
  ([screen possible-moves selected-card card x y]
   (let [border-style (get-card-border-style possible-moves selected-card card)
         suit-style (get-suit-style (:suit card))
         width 5
         height 5
         middle-horizontal-index (+ x 2)
         middle-vertical-index (+ y 2)]
     (draw-box screen x y width height border-style)
     (if (nil? card)
       (s/put-string screen middle-horizontal-index middle-vertical-index (str no-card))
       (if (:flipped card)
         (let [suit-label (str ((:suit card) suit-labels))
               card-label (str (get-card-label card))]
           (s/put-string screen (inc x) (inc y) suit-label suit-style)
           (if (= (:value card) 10)
             (s/put-string screen (dec middle-horizontal-index) middle-vertical-index card-label)
             (s/put-string screen middle-horizontal-index middle-vertical-index card-label))
           (s/put-string screen (inc middle-horizontal-index) (inc middle-vertical-index) suit-label suit-style))
         (dotimes [i (- width 2)]
           (dotimes [j (- height 2)]
             (s/put-string screen (+ i (inc x)) (+ j (inc y)) (str card-back) border-style))))))
   (+ x 5)))

(defn draw-card-right-edge [screen possible-moves card x y]
  (let [style (get-card-border-style possible-moves nil card)]
    (s/put-string screen x y (str top-right-corner) style)
    (dotimes [i 3]
      (s/put-string screen x (+ y i 1) (str vertical-edge) style))
    (s/put-string screen x (+ y 4) (str bottom-right-corner) style))
  (inc x))

(defn draw-card-partly-overlapped-on-left [screen possible-moves card x y]
  (let [border-style (get-card-border-style possible-moves nil card)
        suit-style (get-suit-style (:suit card))
        width 3
        height 5
        middle-vertical-index (+ y 2)]
    (draw-right-semi-box screen x y width height border-style)
    (if (nil? card)
      (s/put-string screen x middle-vertical-index (str no-card))
      (if (:flipped card)
        (let [suit-label (str ((:suit card) suit-labels))
              card-label (str (get-card-label card))]
          (if (= (:value card) 10)
            (s/put-string screen x middle-vertical-index "0")
            (s/put-string screen x middle-vertical-index card-label))
          (s/put-string screen (inc x) (inc middle-vertical-index) suit-label suit-style))
        (dotimes [i (- width 2)]
          (dotimes [j (- height 2)]
            (s/put-string screen (+ i (inc x)) (+ j (inc y)) (str card-back) border-style))))))
  (+ x 3))

(defn draw-card-top-edge [screen possible-moves card x y]
  (let [style (get-card-border-style possible-moves nil card)]
    (s/put-string screen x y (str top-left-corner) style)
    (dotimes [i 3]
      (s/put-string screen (+ x i 1) y (str horizontal-edge) style))
    (s/put-string screen (+ x 4) y (str top-right-corner) style))
  (+ x 5))

(defn draw-stock [screen game-state possible-moves x]
  (let [stock (:stock game-state)]
    (if (empty? stock)
      0
      (if (> x 2)
        (let [top 1]
          (dotimes [i (count stock)]
            (draw-card-right-edge screen possible-moves {:flipped false} (+ i x) top))
          (count stock))
        (let [top 1
              extra-left (draw-card screen possible-moves nil (first stock) x top)]
          (dotimes [i (dec (count stock))]
            (draw-card-right-edge screen possible-moves {:flipped false} (+ i extra-left) top))
          (+ extra-left (dec (count stock))))))))


(defn draw-waste-recur [screen possible-moves x y shown-waste hidden-waste]
  (if (empty? shown-waste)
    (if (empty? hidden-waste)
      nil
      (let [xIndent (draw-card-right-edge screen possible-moves (first hidden-waste) x y)]
        (draw-waste-recur screen
                          possible-moves
                          xIndent
                          y
                          shown-waste
                          (next hidden-waste))))
    (let [xIndent (draw-card-partly-overlapped-on-left screen possible-moves (first shown-waste) x y)]
      (draw-waste-recur screen
                        possible-moves
                        xIndent
                        y
                        (next shown-waste)
                        hidden-waste))))

(defn draw-waste
  ([screen game-state possible-moves selected-card]
   (let [[shown-waste hidden-waste] (l/queue-split-at 3 (:waste game-state))
         top 1]
     (if (empty? shown-waste)
       (draw-waste screen possible-moves shown-waste hidden-waste top 2)
       (draw-waste screen
                   possible-moves
                   shown-waste
                   hidden-waste
                   top
                   (draw-card screen possible-moves selected-card (first shown-waste) 2 top)))))
  ([screen possible-moves shown-waste hidden-waste top left]
   (draw-waste-recur screen possible-moves left top (next shown-waste) hidden-waste)
   (+ left (* 3 (count (next shown-waste))) (count hidden-waste))))

(defn draw-foundations [cli game-state possible-moves selected-card]
  (let [foundations (:foundations game-state)]
    (draw-card (:screen cli) possible-moves selected-card (first (:spades foundations)) (:spades (:foundation-positions cli)))
    (draw-card (:screen cli) possible-moves selected-card (first (:hearts foundations)) (:hearts (:foundation-positions cli)))
    (draw-card (:screen cli) possible-moves selected-card (first (:clubs foundations)) (:clubs (:foundation-positions cli)))
    (draw-card (:screen cli) possible-moves selected-card (first (:diamonds foundations)) (:diamonds (:foundation-positions cli)))))

(defn draw-tableau-recur [screen possible-moves selected-card tableau current-index]
  (if (>= current-index (count tableau))
    nil                                                     ; Done
    (let [cards (reverse (get tableau current-index))
          left (+ (* current-index 7) 2)
          last-top (+ (count cards) 5)]
      (dotimes [i (dec (count cards))]
        (draw-card-top-edge screen possible-moves (nth cards i) left (+ i 6)))
      (if (not (empty? cards))
        (do
          (draw-card screen possible-moves selected-card (last cards) left last-top)
          (draw-tableau-recur screen possible-moves selected-card tableau (inc current-index)))
        (draw-tableau-recur screen possible-moves selected-card tableau (inc current-index))))))

(defn draw-tableau [screen game-state possible-moves selected-card]
  (draw-tableau-recur screen possible-moves selected-card (:tableau game-state) 0))

(defn draw-screen [cli]
  (s/clear (:screen cli))
  (draw-border cli)
  (draw-commands cli)
  (s/redraw (:screen cli)))

(defn draw-game-state [cli game-state possible-moves selected-card]
  (let [x (draw-waste (:screen cli) game-state possible-moves selected-card)]
    (draw-stock (:screen cli) game-state possible-moves x))
  (draw-foundations cli game-state possible-moves selected-card)
  (draw-tableau (:screen cli) game-state possible-moves selected-card)
  (s/redraw (:screen cli)))

(defn await-input [command-channel screen]
  (loop [key (s/get-key-blocking screen)]
    (case key \q (do (>!! command-channel :quit))
              \Q (do (>!! command-channel :quit))
              \f (do (>!! command-channel :flip) (recur (s/get-key-blocking screen)))
              \F (do (>!! command-channel :flip) (recur (s/get-key-blocking screen)))
              \r (do (>!! command-channel :redeal) (recur (s/get-key-blocking screen)))
              \R (do (>!! command-channel :redeal) (recur (s/get-key-blocking screen)))
              :enter (do (>!! command-channel :toggle-select-current) (recur (s/get-key-blocking screen)))
              :left (do (>!! command-channel :highlight-previous) (recur (s/get-key-blocking screen)))
              :right (do (>!! command-channel :highlight-next) (recur (s/get-key-blocking screen)))
              (recur (s/get-key-blocking screen)))))

(defrecord Screen [cols rows screen-type]
  component/Lifecycle
  (start [component]
    (let [screen (s/get-screen screen-type {:cols cols
                                            :rows rows
                                            :resize-listener #((constantly (>!! (:command-channel component) :redraw)) %1 %2)})]
      (s/start screen)
      (assoc component :screen screen
                       :await-input-thread-channel (thread (await-input (:command-channel component) screen))
                       :foundation-positions {:spades [37 1] :hearts [44 1] :clubs [51 1] :diamonds [58 1]})))
  (stop [component]
    (s/stop (:screen component))
    (assoc component :screen nil
                     :await-input-thread-channel nil
                     :foundation-positions nil)))