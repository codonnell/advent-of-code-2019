(ns advent-of-code-2019.day2
  (:require [clojure.string :as str]))

(defn step [program index]
  (let [[op x y target] (drop index program)]
    (assoc program target
           ((case op
              1 +
              2 *) (nth program x) (nth program y)))))

(defn execute [program]
  (loop [index 0
         program program]
    (if (= 99 (nth program index))
      program
      (recur (+ 4 index) (step program index)))))

(defn read-input []
  (let [raw-input (mapv #(Integer/parseInt %) (str/split (str/trim input) #","))]
    (assoc raw-input 1 12 2 2)))

(defn task1 []
  (first (execute (read-input))))

(def success-output 19690720)

(defn success-pair? [program [noun verb]]
  (= success-output (first (execute (assoc program 1 noun 2 verb)))))

(defn task2 []
  (let [input-pairs (for [noun (range 100)
                          verb (range 100)]
                      [noun verb])
        program (read-input)
        [noun verb] (first (filter #(success-pair? program %) input-pairs))]
    (+ (* 100 noun) verb)))

(comment
  (execute [1 0 0 0 99])
  (execute [2 3 0 3 99])
  (execute [2 4 4 5 99 0])
  (execute [1 1 1 4 99 5 6 0 99])
  (def input (slurp "day2input"))
  (mapv #(Integer/parseInt %) (str/split (str/trim input) #","))
  (task1)
  (task2)

  )
