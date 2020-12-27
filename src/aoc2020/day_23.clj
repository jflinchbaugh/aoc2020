(ns aoc2020.day-23
  (:require [aoc2020.core :refer :all]
            [clojure.string :as str]))

(def input "247819356")

(defn parse [input]
  (->> input seq (map (comp parse-int str))))

(defn next-lesser [current lst]
  (let [lesser (filter (partial > current) lst)]
    (apply max (if (empty? lesser) lst lesser))))

(defn next-ring [ring]
  (let [current (first ring)
        to-move (->> ring rest (take 3))
        remaining (remove (set to-move) ring)
        destination (next-lesser current remaining)]
    (->>
     (concat
      (take-while (partial not= destination) remaining)
      [destination]
      to-move
      (rest (drop-while (partial not= destination) remaining)))
     cycle
     rest
     (take (count ring)))))

(defn part-1 []
  (let [start (parse input)]
    (->>
      start
      (iterate next-ring)
      (take (inc 100))
      last
      cycle
      (drop-while (partial not= 1))
      rest
      (take (dec (count start)))
      (str/join ""))))

(comment

  (part-1)
;; => "76385429"


  .)
