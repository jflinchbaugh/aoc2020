(ns aoc2020.day-22)

(def player-1 [31
               24
               5
               33
               7
               12
               30
               22
               48
               14
               16
               26
               18
               45
               4
               42
               25
               20
               46
               21
               40
               38
               34
               17
               50])

(def player-2 [1
               3
               41
               8
               37
               35
               28
               39
               43
               29
               10
               27
               11
               36
               49
               32
               2
               23
               19
               9
               13
               15
               47
               6
               44])


(defn play [[s1 s2]]
  (let [[card-1 & deck-1] s1
        [card-2 & deck-2] s2]
    (cond
      (nil? card-1) [s1 s2]
      (nil? card-2) [s1 s2]
      (> card-1 card-2) [(concat deck-1 [card-1 card-2]) deck-2]
      :else [deck-1 (concat deck-2 [card-2 card-1])]
      )))

(defn part-1 []
  (->>
    [player-1 player-2]
    (iterate play)
    (take-while (fn [[s1 s2]] (not (or (empty? s1) (empty? s2)))))
    last
    play
    first
    reverse
    (interleave (map inc (range)))
    (partition 2)
    (map #(apply * %))
    (reduce +)
    ))

(comment

  (part-1)
;; => 36257

  .)
