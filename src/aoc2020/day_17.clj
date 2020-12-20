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
  (->> input str/trim str/split-lines))

(defn to-cube [ndims lsts]
  (->> (for [y (range (count lsts))
             x (range (count (first lsts)))]
         (when (= \# (nth (nth lsts y) x))
           (vec (concat [x y] (repeat (- ndims 2) 0)))))
       (filter identity)
       set))

(defn surrounding-positions [pos]
  (let [ndims (count pos)
        [ox oy oz] pos]
    (remove #{pos}
      (for [x (range (dec ox) (+ 2 ox))
            y (range (dec oy) (+ 2 oy))
            z (range (dec oz) (+ 2 oz))]
        [x y z]))))

(defn surrounding-positions-4 [pos]
  (let [ndims (count pos)
        [ox oy oz ow] pos]
    (remove #{pos}
      (for [x (range (dec ox) (+ 2 ox))
            y (range (dec oy) (+ 2 oy))
            z (range (dec oz) (+ 2 oz))
            w (range (dec ow) (+ 2 ow))]
        [x y z w]))))

(defn count-surrounding [cube pos]
  (let [ndims (count pos)
        sur-pos (case ndims
                  3 surrounding-positions
                  4 surrounding-positions-4)
        surroundings (filter identity (map cube (sur-pos pos)))]
    (count surroundings)))

(defn candidate-positions [cube]
  (let [ndims (-> cube first count)
        sur-pos (case ndims
                  3 surrounding-positions
                  4 surrounding-positions-4)]
    (->> cube (map sur-pos) (reduce concat cube) set)))

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
  (->> input parse (to-cube 3) (iterate next-cube) (take (inc 6)) last count))

(defn part-2 []
  (->> input parse (to-cube 4) (iterate next-cube) (take (inc 6)) last count))

(comment

  (part-1)
;; => 380

  (part-2)
;; => 2332

  )
