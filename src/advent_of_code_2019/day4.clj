(ns advent-of-code-2019.day4
  (:require [clojure.string :as str]))

(def input [246515 739105])

(defn two-adjacent-match? [n-seq]
  (some #(apply = %) (partition 2 1 n-seq)))

(defn two-but-not-three-adjacent-match? [n-seq]
  (some #(= 2 (count %)) (partition-by identity n-seq)))

(defn non-decreasing? [n-seq]
  (apply <= n-seq))

(defn int->int-seq [n]
  (->> n str (map (comp #(Integer/parseInt %) str))))

(defn task1 []
  (->> input
    (apply range)
    (mapv int->int-seq)
    (filterv #(and (two-adjacent-match? %) (non-decreasing? %)))
    count))

(defn task2 []
  (->> input
    (apply range)
    (mapv int->int-seq)
    (filterv #(and (two-but-not-three-adjacent-match? %) (non-decreasing? %)))
    count))

(comment
  (two-adjacent-match? [1 1 1 1 1 1])
  (two-adjacent-match? [1 2 3 4 5])
  (non-decreasing? [1 2 1 1 1])
  (int->int-seq 123456)
  (task1)
  (not= 1 2 1)
  (distinct? 1 2 1)
  (two-but-not-three-adjacent-match? [1 2 3 4 4 4])
  (two-but-not-three-adjacent-match? [1 1 1 1 2 2])
  (task2)

  )
