(ns advent-of-code-2019.day12
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as math]))

(defn read-input []
  (->> "day12input"
    slurp
    str/split-lines
    (mapv #(re-find #"<x=(.+), y=(.+), z=(.+)>" %))
    (mapv (fn [[_ & groups]] (mapv #(Integer/parseInt %) groups)))))

(defn initial-state [positions]
  (into {}
    (map-indexed (fn [idx position]
                   [idx {:position position :velocity [0 0 0]}]))
    positions))

(defn update-pair-velocity-axis [moons idx1 moon1 moon2 axis]
  (update-in moons [idx1 :velocity axis]
    (case (compare
            (get-in moon1 [:position axis])
            (get-in moon2 [:position axis]))
      -1 inc
      1 dec
      0 identity)))

(defn update-pair-velocities [moons [[idx1 moon1] [idx2 moon2]]]
  (-> moons
    (update-pair-velocity-axis idx1 moon1 moon2 0)
    (update-pair-velocity-axis idx1 moon1 moon2 1)
    (update-pair-velocity-axis idx1 moon1 moon2 2)
    (update-pair-velocity-axis idx2 moon2 moon1 0)
    (update-pair-velocity-axis idx2 moon2 moon1 1)
    (update-pair-velocity-axis idx2 moon2 moon1 2)))

(defn update-velocities [moons]
  (let [pairs (combo/combinations moons 2)]
    (reduce update-pair-velocities moons pairs)))

(defn update-position [{:keys [velocity] :as moon}]
  (update moon :position #(mapv + velocity %)))

(defn update-positions [moons]
  (into {}
    (map (fn [[idx moon]]
           [idx (update-position moon)]))
    moons))

(defn step [moons]
  (-> moons update-velocities update-positions))

(defn kinetic-energy [velocity]
  (transduce (map #(Math/abs %)) + 0 velocity))

(def potential-energy kinetic-energy)

(defn moon-energy [{:keys [position velocity]}]
  (* (kinetic-energy velocity) (potential-energy position)))

(defn total-energy [moons]
  (transduce (map (comp moon-energy val)) + 0 moons))

(defn task1 []
  (->> (read-input)
    initial-state
    (iterate step)
    (drop 231614)
    first
    ;; total-energy
    ))

(defn axis-data [moons axis]
  (into {}
    (map (fn [[idx moon]]
           [idx (-> moon
                  (update :position nth axis)
                  (update :velocity nth axis))]))
    moons))

;; Brute force!?!?!?!
(defn task2 []
  (reduce (fn [{:keys [x-states y-states z-states
                       x-iter y-iter z-iter
                       iter-num] :as acc}
               moons]
            (let [x-data (axis-data moons 0)
                  y-data (axis-data moons 1)
                  z-data (axis-data moons 2)]
              (if (and x-iter y-iter z-iter)
                (reduced [x-iter y-iter z-iter (reduce math/lcm [x-iter y-iter z-iter])])
                (cond-> (update acc :iter-num inc)
                  (and (not x-iter) (contains? x-states x-data))
                  (assoc :x-iter iter-num)
                  (and (not x-iter) (not (contains? x-states x-data)))
                  (update :x-states conj x-data)

                  (and (not y-iter) (contains? y-states y-data))
                  (assoc :y-iter iter-num)
                  (and (not y-iter) (not (contains? y-states y-data)))
                  (update :y-states conj y-data)

                  (and (not z-iter) (contains? z-states z-data))
                  (assoc :z-iter iter-num)
                  (and (not z-iter) (not (contains? z-states z-data)))
                  (update :z-states conj z-data))))
            )
    {:iter-num 0 :x-states #{} :y-states #{} :z-states #{}}
    (iterate step (initial-state (read-input)))))

(comment
  (re-find #"<x=(.+), y=(.+), z=(.+)>" "<x=-1, y=0, z=2>")
  (-> (read-input) initial-state (axis-data 0))
  (task1)
  (task2)
  (reduce math/lcm 1 [231614 193052 102356])
  )
