(ns advent-of-code-2019.day1
  (:require [clojure.string :as str]))

(defn module-fuel [mass]
  (- (quot mass 3) 2))

(defn total-fuel [mass]
  (->> mass
       (iterate module-fuel)
       (drop 1)
       (take-while pos?)
       (reduce + 0)))

(defn read-input []
  (->> "day1input"
       slurp
       str/split-lines
       (mapv #(Integer/parseInt %))))

(defn task1 []
  (let [masses (read-input)]
    (transduce (map module-fuel) + 0 masses)))

(defn task2 []
  (let [masses (read-input)]
    (transduce (map total-fuel) + 0 masses)))

(comment
  (quot 5 3)
  (module-fuel 1969)

  (def input (slurp "day1input"))
  input
  (str/split-lines input)
  (mapv #(Integer/parseInt %) (str/split-lines input))
  (task1)
  (task2)
  )
