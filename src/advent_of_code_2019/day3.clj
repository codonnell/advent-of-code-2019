(ns advent-of-code-2019.day3
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn read-input []
  (->> "day3input"
       slurp
       str/split-lines
       (mapv #(str/split % #","))
       (mapv (fn [wire-directions]
               (mapv (fn [direction]
                       [(keyword (subs direction 0 1))
                        (Integer/parseInt (subs direction 1))])
                     wire-directions)))))

(defn add-coordinates [{:keys [current-distance] [x y] :current :as state} [cardinal length]]
  (let [[x' y' :as new-coordinate] (case cardinal
                                     :R [(+ x length) y]
                                     :L [(- x length) y]
                                     :U [x (+ y length)]
                                     :D [x (- y length)])
        new-wire-coordinates (for [new-x (range (min x x') (inc (max x x')))
                                   new-y (range (min y y') (inc (max y y')))]
                               (with-meta [new-x new-y]
                                 {:distance (+ current-distance
                                               (Math/abs (- x' x))
                                               (Math/abs (- y' y)))}))]
    (-> state
        (update :wire-coordinates set/union (set new-wire-coordinates))
        (update :current-distance + length)
        (assoc :current new-coordinate))))

(defn get-wire-coordinates [paths]
  (:wire-coordinates
   (reduce add-coordinates
           {:current [0 0] :current-distance 0 :wire-coordinates #{}}
           paths)))

(defn task1 []
  (let [directions (read-input)
        [wire1-coordinates wire2-coordinates] (mapv get-wire-coordinates directions)]
    (->> (set/intersection (disj wire1-coordinates [0 0])
                           (disj wire2-coordinates [0 0]))
         (mapv (fn [[x y]]
                 (+ (Math/abs x) (Math/abs y))))
         (apply min))))

(defn get-path-coordinates [[x y] [cardinal length]]
  (let [[x' y' :as new-coordinate] (case cardinal
                                     :R [(+ x length) y]
                                     :L [(- x length) y]
                                     :U [x (+ y length)]
                                     :D [x (- y length)])]
    (drop 1
          (for [new-x (case cardinal
                        :R (range x (inc x'))
                        :L (range x (dec x') -1)
                        (:U :D) [x])
                new-y (case cardinal
                        :U (range y (inc y'))
                        :D (range y (dec y') -1)
                        (:R :L) [y])]
            [new-x new-y]))))

(defn add-distances [{:keys [current] :as state} path]
  (let [path-coordinates (get-path-coordinates current path)
        final-state
        (reduce (fn [{:keys [current-distance distances] :as state*} new-coordinate]
                  (cond-> (update state* :current-distance inc)
                    (not (contains? distances new-coordinate))
                    (update :distances assoc new-coordinate (inc current-distance))))
                state path-coordinates)]
    (assoc final-state :current (last path-coordinates))))

(defn get-wire-distances [paths]
  (:distances
   (reduce add-distances
           {:current [0 0] :current-distance 0 :distances {[0 0] 0}}
           paths)))

(defn task2 []
  (let [directions (read-input)
        [wire1-distances wire2-distances] (mapv get-wire-distances directions)
        intersection-coordinates (set/intersection (set (keys (dissoc wire1-distances [0 0])))
                                                   (set (keys (dissoc wire2-distances [0 0]))))
        coordinate-steps (fn [coordinate]
                           (+ (get wire1-distances coordinate)
                              (get wire2-distances coordinate)))]
    (apply min (mapv coordinate-steps intersection-coordinates))))

(comment
  (keyword "E")
  (range 5 -5)
  (Math/abs -1)
  (task1)
  (task2)
  (def input (read-input))
  (def wire1-distances (get-wire-distances (first input)))
  (def wire2-distances (get-wire-distances (second input)))
  (set/intersection (set (keys (dissoc wire1-distances [0 0])))
                    (set (keys (dissoc wire2-distances [0 0]))))
  (get wire1-distances [1 0])
  (get wire2-distances [157 105])
  (get wire1-distances [107 47])
  (get wire2-distances [107 47])
  (+ 107 47)
  (merge #{[0 0]} #{[1 1]})
  (set/intersection {:a :c :b :d} {:a :d :c :e})
  (meta (get (set/union #{(with-meta [0 0] {:a :b})} #{(with-meta [0 0] {:a :c})})
             [0 0]))
  (range 10 0 -1)

  )
