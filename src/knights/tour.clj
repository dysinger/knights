(ns knights.tour
  (:gen-class)
  (:use clojure.pprint))

(defn board
  "Generate a chess board as a size*size vector of zeros"
  [size]
  (vec (repeat size (vec (repeat size 0)))))

(defn square
  "Retrieve the value of the chessboard square at [vert horz]"
  [board [vert horz]]
  (get (get board vert) horz))

(defn update
  "Update the chess board at [vert horz] to the move value"
  [board [vert horz] move]
  (assoc board vert (assoc (get board vert) horz move)))

(defn moves
  "Find all the moves that can be taken from [vert horz]"
  [board [vert horz]]
  (filter
   (fn [p] (= 0 (square board p)))
   (map (fn [[v h]] [(+ v vert) (+ h horz)])
        [[ 1  2] [ 1 -2] [-1  2] [-1 -2] [ 2  1] [ 2 -1] [-2  1] [-2 -1]])))

(defn best
  "Find the move with the least possible moves"
  [board pos]
  (let [m (moves board pos)]
    (when-not (empty? m)
      (apply min-key (fn [p] (count (moves board p))) m))))

(defn solve
  "Solve the knights open-tour problem"
  ([board pos count]
     (let [new-board (update board pos count)]
       (if-let [new-move (best board pos)]
         (solve new-board new-move (+ 1 count))
         new-board)))
  ([] (solve (board 8) [7 0] 1)))

(defn -main [& args]
  (println (pprint (time (solve)))))
