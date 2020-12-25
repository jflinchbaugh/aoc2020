(ns aoc2020.day-24
  (:require  [clojure.string :as str]))

(def input (slurp "src/aoc2020/day_24.txt"))

(defn parse [input]
  (->>
    input
    str/trim
    str/split-lines
    (map (partial re-seq #"e|se|sw|w|nw|ne"))))

(defn count-dirs [steps]
  (->>
    steps
    (group-by identity)
    (map (fn [[k v]] [k (count v)]))
    (into {})))

(defn normalize
  "normalize a 3-dimensional hex position to 2-dimensional diagonal"
  [counts]
  (let [x (- (counts "e" 0) (counts "w" 0))]
    [(- (counts "se" 0) (counts "nw" 0) (* -1 x))
     (- (counts "ne" 0) (counts "sw" 0 ) (* -1 x))]))

(defn adjacent-cells [[a b]]
  (for [x (range -1 2)
        y (range -1 2)
        :when (not (#{[-1 1] [1 -1] [0 0]} [x y]))]
    [(+ a x) (+ b y)]))

(defn candidate-cells [cells]
  (let [adjacent (map adjacent-cells cells)]
    (->> adjacent (cons cells) (reduce concat) set)))

(defn part-1 []
  (->>
    input
    parse
    (map count-dirs)
    (map normalize)
    (group-by identity)
    (map (fn [[k v]] [k (count v)]))
    (filter (fn [[k v]] (odd? v)))
    count
    ))

(defn next-is-black? [cells cell]
  (let [surrounding (->> cell adjacent-cells (filter cells) count)]
    (if (nil? (cells cell))
      (= 2 surrounding)
      (#{1 2} surrounding)
      )))

(defn next-state [cells]
  (let [candidates (candidate-cells cells)]
    (->> candidates (filter (partial next-is-black? cells)) set)))

(defn part-2 []
  (let [cells (->>
                input
                parse
                (map count-dirs)
                (map normalize)
                (group-by identity)
                (map (fn [[k v]] [k (count v)]))
                (filter (fn [[k v]] (odd? v)))
                (map first)
                set)]
    (->> cells (iterate next-state) (take (inc 100)) last count)))

(comment

  ;;     -Y  +X
  ;;       \ /
  ;; -X,-Y -0- +X,+Y
  ;;       / \
  ;;     -X  +Y


  (part-1)
;; => 282

  (part-2)
;; => 3445

  .)
