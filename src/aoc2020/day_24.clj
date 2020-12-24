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

(defn normalize [counts]
  (let [x (- (counts "e" 0) (counts "w" 0))]
    [(- (counts "se" 0) (counts "nw" 0) (* -1 x))
     (- (counts "ne" 0) (counts "sw" 0 ) (* -1 x))]))
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

(comment

  (part-1)
;; => 282

  .)
