(ns aoc2020.day-18
  (:require [aoc2020.core :refer :all]
            [clojure.string :as str]))

(def input (slurp "src/aoc2020/day_18.txt"))

(defn transform-line [tokens]
  (map
   (fn [v]
     (cond
       (re-matches #"\d" (str v)) (parse-int (str v))
       :else v))
   tokens))

(defn calc [[p1 op p2]]
  (let [operator ({\+ +, \* *} op)
        _ (prn p1 op p2)]
    (operator p1 p2)))

(defn walk [tokens token]
  (cond
    (#{\(} token) tokens
    (#{\)} token) tokens
    :else (let [new-tokens (conj tokens token)]
      (if (= 3 (count new-tokens))
        [(calc new-tokens)]
        new-tokens))))

(comment

  (->>
    input
    str/trim
    str/split-lines
    (map (partial remove #{\space}))
    (map transform-line)
    first
    (reduce walk []))

  (reduce walk [] [1 \+ 2 \* 3])

  .)
