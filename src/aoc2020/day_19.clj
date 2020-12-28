(ns aoc2020.day-19
  (:require [clojure.string :as str]))

(def input (slurp "src/aoc2020/day_19.txt"))

(defn is-rule? [s] (re-matches #"\d+:.*" s))

(defn parse [input] (->> input str/trim str/split-lines (remove empty?)))

(defn parse-branches [rule]
  (->> rule rest (map #(str/split % #" "))))

(defn parse-rule [s]
  ((juxt
    first
    parse-branches)
   (str/split s #" *[:|] *")))

(defn part-1 []
  (let [lines (parse input)
        rules (filter is-rule? lines)
        signals (remove is-rule? lines)]
    (->> rules (map parse-rule) (into {}))))

(comment

  (part-1)

  .)
