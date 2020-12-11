(ns aoc2020.day-10)

(def input [49
            89
            70
            56
            34
            14
            102
            148
            143
            71
            15
            107
            127
            165
            135
            26
            119
            46
            53
            69
            134
            1
            40
            81
            140
            160
            33
            117
            82
            55
            25
            11
            128
            159
            61
            105
            112
            99
            93
            151
            20
            108
            168
            2
            109
            75
            139
            170
            65
            114
            21
            92
            106
            162
            124
            158
            38
            136
            95
            161
            146
            129
            154
            121
            86
            118
            88
            50
            48
            62
            155
            28
            120
            78
            60
            147
            87
            27
            7
            54
            39
            113
            5
            74
            169
            6
            43
            8
            29
            18
            68
            32
            19
            133
            22
            94
            47
            132
            59
            83
            12
            13
            96
            35])

(defn part-1 [input]
  (->>
   input
   (cons 0)
   (cons (+ 3 (apply max input)))
   sort
   (partition 2 1)
   (map #(reduce - (reverse %)))
   sort
   (partition-by identity)
   (map count)
   (reduce *)))

(defn count-combos [i]
  (if (>= 1 i) 1
      (+ (dec i) (count-combos (dec i)))))

(defn reducible? [p]
  (and (< 1 (count p)) (not= 3 (first p))))

(defn to-steps [coll]
  (->>
   coll
   (partition 2 1)
   (map #(reduce - (reverse %)))))

(defn part-2 [input]
  (->>
   input
   (cons 0)
   (cons (+ 3 (apply max input)))
   sort
   to-steps
   (partition-by identity)
   (filter reducible?)
   (map count)
   (map count-combos)
   (reduce *)))

(comment
  (part-1 input)

;; => 2414

  (part-2 input)
;; => 21156911906816
  )
