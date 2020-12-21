(ns aoc2020.day-20
  (:require [aoc2020.core :refer :all]
            [clojure.string :as str]))

(def input (slurp "src/aoc2020/day_20.txt"))

(def sample "
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
")

(def tile-size 10)

(defn to-cells [lsts]
  (->> (for [y (range (count lsts))
             x (range (count (first lsts)))]
         (when (= \# (nth (nth lsts y) x))
           [x y]))
       (filter identity)
       set))

(defn parse-tile [[tile-name & lines]]
  (let [tile-number (-> tile-name (str/replace #"Tile |:" "") parse-int)
        occupied-cells (to-cells lines)]
    [tile-number occupied-cells]))

(defn parse [input]
  (->>
   input
   str/trim
   str/split-lines
   (partition-by #{""})
   (remove #(= 1 (count %)))
   (map parse-tile)))

(defn to-edges [cells]
  (let [top (for [x (range tile-size)
                  y [0]]
              (boolean (cells [x y])))
        right (for [x [(dec tile-size)]
                    y (range tile-size)]
                (boolean (cells [x y])))
        bottom (for [x (reverse (range tile-size))
                     y [(dec tile-size)]]
                 (boolean (cells [x y])))
        left (for [x [0]
                   y (reverse (range tile-size))]
               (boolean (cells [x y])))]
    [top right bottom left]))

(defn with-edges [[name cells]] [name (to-edges cells)])

(defn edges-match? [e1 e2]
  (or (= e1 (reverse e2)) (= e1 e2)))

(defn num-inside-edges [n] (* 2 n (- n 1)))

(defn part-1 [] 
  (let [tiles (->>
                input
                parse
                (map with-edges))
        ntiles (count tiles)
        matching-edges (->> (for [t1 tiles
                                  t2 tiles
                                  e1 (second t1)
                                  e2 (second t2)]
                              [[(first t1) (first t2)] e1 e2])
                         (remove (fn [[[t1 t2]]] (= t1 t2)))
                         (filter (fn [[_ e1 e2]] (edges-match? e1 e2))))]
    (->>
      matching-edges
      (group-by (comp first first))
      (map (fn [[k v]] [k (count v)]))
      (filter #(= 2 (second %)))
      (map first)
      (reduce *))))

(comment

  (part-1)
;; => 18411576553343

  (num-inside-edges 3)


  .)
