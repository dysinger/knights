(ns knights.tour
  (:gen-class)
  (:use clojure.pprint))

(defn board
  "Generate a chess board as a size*size vector of zeros. Use get-in
  and assoc-in to access/update the squares."
  [size]
  (vec (repeat size (vec (repeat size 0)))))

(defn moves
  "Find all the moves that can be taken from [vert horz]"
  [board [vert horz]]
  (for [[v h] [[ 1  2] [ 1 -2] [-1  2] [-1 -2] [ 2  1] [ 2 -1] [-2  1] [-2 -1]]
        :let [p [(+ v vert) (+ h horz)]]
        :when (= 0 (get-in board p))]
    p))

(defn best
  "Find the move with the least possible moves"
  [board pos]
  (when-let [m (seq (moves board pos))]
    (apply min-key (fn [p] (count (moves board p))) m)))

(defn solve
  "Solve the knights open-tour problem"
  ([board pos count]
     (let [new-board (assoc-in board pos count)]
       (if-let [new-move (best board pos)]
         (recur new-board new-move (+ 1 count))
         new-board)))
  ([] (solve (board 8) [7 0] 1)))

(defn -main [& args]
  (println (pprint (time (solve)))))
