(ns aoc2020.day-15)

(def input [2,15,0,9,1,20])

(defn catalog [[m sz l] n]
  [(if l (assoc m l sz) m) (inc sz) n])

(defn init-catalog [input]
  (reduce catalog  [{} -1 nil] input))

(defn next-num-catalog [[m sz last-num]]
  (let [found (m last-num)]
    (if found (- sz found) 0)))

(defn catalog-next [[m sz last-num]]
  (catalog [m sz last-num] (next-num-catalog [m sz last-num])))

(defn part-1 []
  (->
    (iterate catalog-next input)
    (nth (- 2020 (count input)))
    last))

(defn part-1 []
  (->
    (iterate catalog-next (init-catalog input))
    (nth (- 2020 (count input)))
    last))

(defn part-2 []
  (->
    (iterate catalog-next (init-catalog input))
    (nth (- 30000000 (count input)))
    last))

(comment

  (part-1)
;; => 1280

  (part-2)
;; => 651639

  )
