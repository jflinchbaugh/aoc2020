(ns aoc2020.day-05-test
  (:require [aoc2020.day-05 :as sut]
            [clojure.test :refer :all]))

(deftest seat-number-test
  (testing "seat number with samples"
    (are [seat-number bi-space-part]
        (= seat-number (sut/seat-number bi-space-part))
      567 "BFFFBBFRRR"
      119 "FFFBBBFRRR"
      820 "BBFFBBFRLL")))

(deftest seat-between-test
  (testing "seat number with samples"
    (are [seat-1 seat-2 between]
        (= between (sut/seat-between? [seat-1 seat-2]))
      1 2 false
      1 3 true
      1 4 false)))

