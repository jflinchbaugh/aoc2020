(ns aoc2020.day-17
  (:require [aoc2020.core :refer :all]
            [clojure.string :as str]))

(def input "
#.#.##.#
#.####.#
...##...
#####.##
#....###
##..##..
#..####.
#...#.#.
")

(defn parse [input]
  (->> input str/trim str/split-lines (map seq)))

(defn to-cube [lsts]
  (->> (for [y (range (count lsts))
             x (range (count (first lsts)))]
         (when (= \# (nth (nth lsts y) x)) [x y 0]))
       (filter identity)
       set))

(defn surrounding-positions [[ox oy oz]]
  (remove #{[ox oy oz]}
          (for [x (range (dec ox) (+ 2 ox))
                y (range (dec oy) (+ 2 oy))
                z (range (dec oz) (+ 2 oz))]
            [x y z])))

(defn count-surrounding [cube pos]
  (let [surroundings (filter identity (map cube (surrounding-positions pos)))]
    (count surroundings)))

(defn candidate-positions [cube]
  (->> cube (map surrounding-positions) (reduce concat cube) set))

(defn next-val [cube pos]
  (let [cnt (count-surrounding cube pos)
        cur-val (boolean (cube pos))]
    (if cur-val
      (boolean (#{2 3} cnt))
      (= cnt 3))
    ))

(defn next-cube [cube]
  (->>
    cube
    candidate-positions
    (map #(when (next-val cube %) %))
    (filter identity)
    set))

(defn part-1 []
  (->> input parse to-cube (iterate next-cube) (take (inc 6)) last count))

(comment

  (part-1)
;; => 380

  )
