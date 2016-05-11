(ns clondike.game.logic)

(require '[com.rpl.specter :as s])

(def ^:const all-suits #{:spades :hearts :clubs :diamonds})

(def ^:const red-suits #{:hearts :diamonds})

(def ^:const black-suits #{:spades :clubs})

(defrecord Card [value
                 suit
                 flipped])

(defrecord Foundation [spades                               ;; PersistentList
                       hearts                               ;; PersistentList
                       clubs                                ;; PersistentList
                       diamonds])                           ;; PersistentList

(defrecord Board [stock                                     ;; PersistentQueue
                  waste                                     ;; PersistentList (Stack)
                  tableau                                   ;; PersistentVector of PersistentLists
                  foundation])

;; TODO: consolidate by using Specter paths
(defrecord Move [card from to])

(defn make-queue [coll]
  (reduce
    #(conj %1 %2)
    clojure.lang.PersistentQueue/EMPTY
    coll))

(defn generate-shuffled-deck []
  (make-queue
    (shuffle
      (mapcat (fn [suit]
                (map
                  #(->Card %1 suit false)
                  (range 1 14)))                            ;; 1 = Ace, 13 = King
              all-suits))))

(defn flip-card [^Card card flipped]
  (assoc card :flipped flipped))

(defn flip-cards [cards flipped]
  (reduce
    #(conj %1 (flip-card %2 flipped))
    clojure.lang.PersistentQueue/EMPTY
    cards))

(defn make-tableau-recur [cards tableau current-index]
  ;; Check if we have moved past the last position in the tableau
  (if (>= current-index (count tableau))
    ;; Check if we are finished
    (if (>= (count (get tableau (dec (count tableau)))) (count tableau))
      tableau                                               ; Tableau finished!
      (make-tableau-recur cards tableau 0))                 ; Move back to first position
    ;; Decide whether to add a card or move to the next position
    (let [column-count (count (get tableau current-index))]
      (if (<= column-count current-index)
        (make-tableau-recur
          (next cards)
          (assoc
            tableau
            current-index
            (conj
              (get tableau current-index)
              (if (= column-count current-index)
                (flip-card (first cards) true)
                (flip-card (first cards) false))))
          (inc current-index))
        (make-tableau-recur cards tableau (inc current-index))))))

(defn make-tableau [cards]
  "The tableau consists of the stacks of cards the user can move, build on,
  and take from. This function creates the tableau with the given cards."
  (make-tableau-recur cards ['() '() '() '() '() '() '()] 0))

(defn partial-sum [n]
  (/ (* (inc n) n) 2))

(def ^:const initial-tableau-size (partial-sum 7))

(defn queue-split-at-recur [n a b]
  (if (or (zero? n) (empty? b))
    [a b]
    (queue-split-at-recur (dec n) (conj a (first b)) (next b))))

(defn queue-split-at [n l]
  (queue-split-at-recur n clojure.lang.PersistentQueue/EMPTY l))

(defn make-game-state [cards]
  (let [[tableau-cards stock-cards] (queue-split-at initial-tableau-size cards)]
    {:tableau     (make-tableau tableau-cards)
     :stock       stock-cards
     :waste       '()
     :foundations {:spades   '()
                   :hearts   '()
                   :clubs    '()
                   :diamonds '()}}))

(defn solved? [^Board board]
  (and (empty? (:waste board))
       (empty? (:stock board))
       (empty? (s/select [s/ALL #(not (empty? %1))] (:tableau board)))))

(defn can-build? [^Card new-card ^Card existing-card]
  (and
    (= (:value new-card) (dec (:value existing-card)))
    (or
      (and (contains? red-suits (:suit new-card))
           (contains? black-suits (:suit existing-card)))
      (and (contains? black-suits (:suit new-card))
           (contains? red-suits (:suit existing-card))))))

(defn can-move-card-to-tableau-stack? [^Card card tableau-stack]
  (or (and
        (empty? tableau-stack)
        (= (:value card) 13))                               ;; 13 = King
      (and
        (not (empty? tableau-stack))
        (can-build? card (first tableau-stack)))))

(defn which-card-can-move-from-tableau-stack? [^Board board from to]
  (let [destination-stack (nth (:tableau board) to)
        source-stack (nth (:tableau board) from)]
    (if (empty? source-stack)
      nil
      (first (filter
               #(and (:flipped %1)
                     (can-move-card-to-tableau-stack? %1 destination-stack))
               source-stack)))))

(defn can-move-to-foundation? [^Board board ^Card card]
  (if (nil? card)
    false
    (let [foundation-top
          (first ((:suit card) (:foundations board)))]
      (if (nil? foundation-top)
        (= (:value card) 1)                                 ;; 1 = Ace
        (= (:value card) (inc (:value foundation-top)))))))

(defn can-move-foundation-to-tableau-stack? [foundation tableau-stack]
  (and (not (empty? foundation))
       (not (empty? tableau-stack))
       (can-build? (first foundation) (first tableau-stack))))

(defn possible-stock-moves [^Board board]
  (if (or (not (empty? (:stock board)))
          (not (empty? (:waste board))))
    (list (->Move nil :stock :waste))
    nil))

(defn possible-waste-tableau-moves [^Board board]
  (if (empty? (:waste board))
    '()
    (let [top-card (first (:waste board))]
      (for [n (range (count (:tableau board)))]
        (if (can-move-card-to-tableau-stack?
              top-card
              (nth (:tableau board) n))
          (->Move top-card :waste n)
          nil)))))

(defn possible-waste-foundation-moves [^Board board]
  (if (and
        (not (empty? (:waste board)))
        (can-move-to-foundation? board (first (:waste board))))
    (list (->Move (first (:waste board)) :waste (:suit (first (:waste board)))))
    nil))

(defn possible-tableau-stack-moves [^Board board]
  (apply concat
         (concat
           (for [from (range (dec (count (:tableau board))))]
             (for [to (range (inc from) (count (:tableau board)))]
               (let [card (which-card-can-move-from-tableau-stack? board from to)]
                 (if (nil? card)
                   nil
                   (->Move card from to)))))
           (for [from (range (dec (count (:tableau board))))]
             (for [to (range (inc from) (count (:tableau board)))]
               (let [card (which-card-can-move-from-tableau-stack? board to from)]
                 (if (nil? card)
                   nil
                   (->Move card to from))))))))

(defn possible-tableau-foundation-moves [^Board board]
  (for [from (range (count (:tableau board)))]
    (let [card (first (nth (:tableau board) from))]
      (if (can-move-to-foundation? board card)
        (->Move card from (:suit card))
        nil))))

(defn possible-foundation-moves [^Board board]
  (apply concat
         (for [from (range (count (:tableau board)))]
           (apply list (map
                         #(let [foundation (%1 (:foundations board))
                                tableau-stack (nth (:tableau board) from)]
                           (if (can-move-foundation-to-tableau-stack? foundation tableau-stack)
                             nil))
                         (keys (:foundations board)))))))

(defn possible-moves [^Board board]
  (filter #(not (nil? %1))
          (concat
            (possible-stock-moves board)
            (possible-waste-foundation-moves board)
            (possible-waste-tableau-moves board)
            (possible-tableau-stack-moves board)
            (possible-tableau-foundation-moves board)
            (possible-foundation-moves board))))

(defn deal-from-stock-to-waste [stock waste number-of-cards]
  (if (zero? number-of-cards)
    [stock waste]
    (deal-from-stock-to-waste
      (next stock)
      (conj waste (flip-card (first stock) true))
      (dec number-of-cards))))

;; Take three cards from the stock and move them to the waste
(defn deal-cascade [^Board board]
  (let [stock (:stock board)
        waste (:waste board)]
    (if (empty? stock)
      (if (empty? waste)
        board                                               ; No move when stock and waste are empty
        (assoc board
          :stock (flip-cards (reverse waste) false)
          :waste '()))
      (let [[new-stock new-waste] (deal-from-stock-to-waste stock waste 3)] ;; Deal 3
        (assoc board
          :stock new-stock
          :waste new-waste)))))

;; Move the top waste card to a specific column in the tableau
(defn move-waste-to-tableau [^Board board n]
  (let [card (first (:waste board))]
    (assoc board
      :waste (next (:waste board))
      :tableau (assoc
                 (:tableau board)
                 n
                 (conj (nth (:tableau board) n) card)))))

;; TODO Add additional board checks
(defn move-waste-to-foundation [^Board board]
  (let [card (first (:waste board))]
    (let [foundation ((:suit card) (:foundations board))]
      (assoc board
        :waste (next (:waste board))
        :foundations (assoc
                       (:foundations board)
                       (:suit card)
                       (conj foundation card))))))

(defn auto-flip-tableau-stack [tableau-stack]
  (if (or
        (zero? (count tableau-stack))
        (:flipped (first tableau-stack)))
    tableau-stack
    (conj (next tableau-stack) (flip-card (first tableau-stack) true))))

(defn move-tableau-stack [^Board board ^Move move]
  "Move from one tableau stack to another"
  (let [from (:from move)
        to (:to move)
        to-stack (nth (:tableau board) to)
        from-stack (nth (:tableau board) from)]
    (loop [n (dec (count from-stack))]
      (if (neg? n)
        board
        (let [card (nth from-stack n)]
          (if (and
                (:flipped card)
                (can-move-card-to-tableau-stack? card to-stack))
            (let [[moved-cards remaining-cards]
                  (split-at (inc n) from-stack)]
              (assoc board :tableau
                           (assoc (:tableau board)
                             from (auto-flip-tableau-stack
                                    (apply list remaining-cards))
                             to (apply list (concat moved-cards to-stack)))))
            (recur (dec n))))))))

(defn move-from-tableau-to-foundation [^Board board from]
  (let [tableau-card (first (nth (:tableau board) from))]
    (if (can-move-to-foundation? board tableau-card)
      (assoc board
        :foundations
        (assoc (:foundations board)
          (:suit tableau-card)
          (conj
            ((:suit tableau-card) (:foundations board))
            tableau-card))
        :tableau
        (assoc (:tableau board)
          from
          (auto-flip-tableau-stack
            (next (nth (:tableau board) from)))))
      board)))

(defn move-foundation-to-tableau [^Board board ^Move move]
  (let [from (:from move)
        to (:to move)
        tableau-stack (nth (:tableau board) to)
        foundation (from (:foundations board))]
    (if (can-move-foundation-to-tableau-stack? foundation tableau-stack)
      (assoc board
        :tableau (assoc
                   (:tableau board)
                   to
                   (conj tableau-stack (first foundation)))
        :foundations (assoc
                       (:foundations board)
                       from
                       (next foundation)))
      board)))

(defn apply-tableau-stack-move [^Board board ^Move move]
  (let [from (:from move)
        to (:to move)]
    (case to
      :spades (move-from-tableau-to-foundation board from)
      :hearts (move-from-tableau-to-foundation board from)
      :clubs (move-from-tableau-to-foundation board from)
      :diamonds (move-from-tableau-to-foundation board from)
      0 (move-tableau-stack board move)
      1 (move-tableau-stack board move)
      2 (move-tableau-stack board move)
      3 (move-tableau-stack board move)
      4 (move-tableau-stack board move)
      5 (move-tableau-stack board move)
      6 (move-tableau-stack board move)
      board)))

(defn apply-move [^Board board ^Move move]
  (let [from (:from move)
        to (:to move)]
    (case from
      :stock (case to
               :waste (deal-cascade board)
               board)
      :waste (case to
               :spades (move-waste-to-foundation board)
               :hearts (move-waste-to-foundation board)
               :clubs (move-waste-to-foundation board)
               :diamonds (move-waste-to-foundation board)
               (move-waste-to-tableau board to))
      0 (apply-tableau-stack-move board move)
      1 (apply-tableau-stack-move board move)
      2 (apply-tableau-stack-move board move)
      3 (apply-tableau-stack-move board move)
      4 (apply-tableau-stack-move board move)
      5 (apply-tableau-stack-move board move)
      6 (apply-tableau-stack-move board move)
      :spades (move-foundation-to-tableau board move)
      :hearts (move-foundation-to-tableau board move)
      :clubs (move-foundation-to-tableau board move)
      :diamonds (move-foundation-to-tableau board move)
      board)))