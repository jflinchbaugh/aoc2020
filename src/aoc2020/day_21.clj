(ns aoc2020.day-21
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp "src/aoc2020/day_21.txt"))

(defn parse [input]
  (->>
    input
    str/trim
    str/split-lines
    (map #(str/split % #"[,() ]+"))
    (map (partial partition-by (partial = "contains")))
    (map (fn [[ingredients _ allergens]] [allergens ingredients]))
    ))

(defn fan-out [m [allergens ingredients]]
  (concat m (for [a allergens]
              [a ingredients])))

(def known '(["dairy" #{"pgnpx"}]
            ["sesame" #{"nvbrx"}]
            ["wheat" #{"xzb"}]
            ["soy" #{"zbkbgp"}]
            ["peanuts" #{"dskjpq"}]
            ["fish" #{"ksdgk"}]
            ["eggs" #{"srmsh"}]
            ["shellfish" #{"khqsk"}]))

(defn part-1 []
  (->>
    input
    parse
    (map second)
    (reduce concat)
    (remove (set (map (comp first second) known)))
    count))

(defn part-2 []
  (->>
    known
    (sort-by first)
    (map (comp first second))
    (str/join ",")))

(comment

  (->>
    input
    parse
    (reduce fan-out [])
    (group-by first)
    (map
      (fn [[k v]]
        [k (map (comp set second) v)]))
    (map
      (fn [[k v]]
        [k (apply set/intersection v)])))
;; => (["dairy" #{"khqsk" "pgnpx" "ksdgk"}]
;;     ["sesame" #{"khqsk" "pgnpx" "dskjpq" "nvbrx"}]
;;     ["wheat" #{"xzb"}]
;;     ["soy" #{"khqsk" "zbkbgp" "xzb" "nvbrx"}]
;;     ["peanuts" #{"khqsk" "dskjpq"}]
;;     ["fish" #{"khqsk" "ksdgk" "srmsh"}]
;;     ["eggs" #{"dskjpq" "srmsh"}]
;;     ["shellfish" #{"khqsk" "xzb"}])
  ;; TODO process this list by elimination; cheated with `known`

  (part-1)
;; => 1945

  (part-2)
;; => "pgnpx,srmsh,ksdgk,dskjpq,nvbrx,khqsk,zbkbgp,xzb"

  .)
