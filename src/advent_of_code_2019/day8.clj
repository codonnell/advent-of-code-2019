(ns advent-of-code-2019.day8
  (:require [clojure.string :as str]))

(defn get-digits [n]
  (->> n str (mapv (comp #(Integer/parseInt %) str))))

(def width 25)
(def height 6)

(defn read-input []
  (->> "day8input" slurp str/trim get-digits (partition width) (partition height)))

(defn task1 []
  (->> (read-input)
    (sort-by #(count (filter #{0} (apply concat %))))
    first
    ((fn [layer]
       (let [num-ones (count (filter #{1} (apply concat layer)))
             num-twos (count (filter #{2} (apply concat layer)))]
         (* num-ones num-twos))))))

(defn get-pixel [layers x y]
  (loop [layers layers]
    (let [top-pixel (nth (nth (first layers) y) x)]
      (cond (not= 2 top-pixel) top-pixel
            (= 1 (count layers)) 2
            :else (recur (rest layers))))))

(defn task2 []
  (let [input (read-input)]
    (for [y (range height)]
      (for [x (range width)]
        (get-pixel input x y)))))

(comment
  (def input (read-input))
  input
  (task1)
  (nth (range 3) 1)
  (task2)

  )
