(ns aoc2020.day-25)

(def public-keys [15113849
                  4206373])

(defn next-value [s n]
  (mod (* n s) 20201227))

(defn loop-count [k]
  (->> (iterate (partial next-value 7) 1) (take-while #(not= k %)) count))

(defn calc-encryption-key [public-key loop-count]
  (->> (iterate (partial next-value public-key) 1) (take (inc loop-count)) last))

(defn part-1 []
  (let [loop-counts (map loop-count public-keys)]
    (->> loop-counts reverse (map calc-encryption-key public-keys) first)))

(comment
  (part-1)
;; => 1890859

  .)
