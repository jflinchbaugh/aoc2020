(ns aoc2020.day-15)

(def input [2,15,0,9,1,20])

(defn next-num [coll]
  (let [rev (reverse coll)
        length (count coll)
        last-num (first rev)
        prev-nums (rest rev)
        dist-back (inc (count (take-while (complement #{last-num}) prev-nums)))]
    (if (= length dist-back) 0 dist-back))
  )

(defn add-next [coll]
  (conj coll (next-num coll)))

(defn part-1 []
  (->
    (iterate add-next input)
    (nth (- 2020 (count input)))
    last))

(comment

  (part-1)
;; => 1280

  )
