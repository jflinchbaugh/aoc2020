(ns aoc2020.day-13
  (:require [clojure.string :as str]))

(def earliest 1001612)

(def input 
  "19,x,x,x,x,x,x,x,x,41,x,x,x,37,x,x,x,x,x,821,x,x,x,x,x,x,x,x,x,x,x,x,13,x,x,x,17,x,x,x,x,x,x,x,x,x,x,x,29,x,463,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,23")

(defn parse-int [n] (Integer/parseInt n))

(defn parse [input]
  (->> (str/split input #",")))

(defn next-arrival [earliest id] (* id (inc (int (/ earliest id)))))

(defn part-1 []
  (let [ids (->> input parse (remove #{"x"}) (map parse-int))
        [next-arrival-time id] (->> ids
                                 (map #(-> [(next-arrival earliest %) %]))
                                 (sort-by first) first)]
    (* id (- next-arrival-time earliest))))

(comment


  (part-1)
;; => 6568

  )
