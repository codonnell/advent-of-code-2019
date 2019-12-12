(ns advent-of-code-2019.day10
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn read-input []
  (->> (slurp "day10input")
    (str/split-lines)
    (into #{}
      (comp
        (map-indexed (fn [line-idx line]
                       (into #{}
                         (keep-indexed (fn [row-idx v]
                                         (when (= \# v)
                                           [row-idx line-idx])))
                         line)))
        cat))))

(defn points-between [p1 p2]
  (let [[[x1 y1] [x2 y2]] (sort [p1 p2])]
    (if (= x1 x2)
      (map (fn [y] [x1 y]) (range (inc y1) y2))
      (let [slope (/ (- y2 y1) (- x2 x1))]
        (for [x (range (inc x1) x2)
              :let [y (+ y1 (* slope (- x x1)))]
              :when (integer? y)]
          [x y])))))

(defn visible? [asteroids p1 p2]
  (not (some #(contains? asteroids %) (points-between p1 p2))))

(defn compute-visibility-map [asteroids]
  (let [pairs (combo/combinations asteroids 2)]
    (reduce (fn [m [p1 p2]]
              (if (visible? asteroids p1 p2)
                (-> m
                  (update p1 (fnil conj []) p2)
                  (update p2 (fnil conj []) p1))
                m))
      {} pairs)))

(defn task1 []
  (->> (read-input)
    compute-visibility-map
    (into {} (map (fn [[k v]] [k (count v)])))
    (sort-by second (comp - compare))
    first
    second))

(def laser-base [19 14])
(def laser-x 19)
(def laser-y 14)
;; 0 -> pi / 2
;; pi / 2 -> 0
;; pi -> 3pi / 2
;; 3pi / 2 -> pi
(defn laser-angle [[x y]]
  (let [[x' y'] [(- x laser-x) (- (- y laser-y))]
        theta (Math/atan2 y' x')]
    (mod (- (/ Math/PI 2) theta) (* 2 Math/PI))))

(defn task2 []
  (let [[x y] (loop [asteroids (read-input)
                     num-lasered 0]
                (let [visible (->> (disj asteroids laser-base)
                                (filter #(visible? asteroids laser-base %))
                                (sort-by laser-angle))]
                  (if (>= (+ (count visible) num-lasered) 200)
                    (first (drop (- 199 num-lasered) visible))
                    (recur (reduce disj asteroids visible) (+ num-lasered (count visible))))))]
    (+ (* 100 x) y)))

(comment
  (def input (read-input))
  input
  (sort-by second (into {} (map (fn [[k v]] [k (count v)])) (compute-visibility-map input)))
  (integer? (+ (+ 1 1/2) 1/2))
  (for [x (range 10)
        :let [y (+ (/ x 2) 1/2)]
        :when (integer? y)]
    [x y])
  (points-between [1 0] [4 3])
  (sort [[0 0] [1 0] [0 1] [1 1]])
  (task1)
  (mod 1.5 1)
  (laser-angle [19 15])
  (task2)
  )
