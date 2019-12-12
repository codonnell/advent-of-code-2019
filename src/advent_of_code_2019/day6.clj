(ns advent-of-code-2019.day6
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [loom.graph :as g]
            [loom.alg-generic :as alg]))

(defn read-input []
  (->> "day6input"
    slurp
    (str/split-lines)
    (mapv #(str/split % #"\)"))))

(defn create-ancestry [g]
  (reduce (fn [ancestry node]
            (loop [ancestors (set (g/predecessors g node))]
              (let [ancestors' (into #{} (mapcat #(g/predecessors g %)) ancestors)]
                (if (set/subset? ancestors' ancestors)
                  (assoc ancestry node ancestors)
                  (recur (set/union ancestors ancestors'))))))
    {} (g/nodes g)))

(defn task1 []
  (->> (read-input)
    (apply g/digraph)
    create-ancestry
    (transduce (map (fn [[_ ancestors]] (count ancestors))) + 0)))

(defn task2 []
  (let [dg (apply g/digraph (read-input))
        g (g/graph dg)
        im-orbiting (first (g/predecessors dg "YOU"))
        santa-orbiting (first (g/predecessors dg "SAN"))]
    (dec (count (alg/bf-path (g/successors g) im-orbiting santa-orbiting)))))

(comment
  (str/split "ABC)123" #"\)")
  (def input (read-input))
  input
  (def dg (apply g/digraph input))
  (def g (g/graph dg))
  (g/predecessors g "YOU")
  (g/nodes dg)
  (g/successors dg "SKF")
  (create-ancestry (g/digraph [1 2] [2 3] [3 4] [3 5]))
  (def ancestry (create-ancestry dg))
  (reduce + (map count (vals ancestry)))
  (task1)
  (first (g/predecessors dg "YOU"))
  (first (g/predecessors dg "SAN"))
  (alg/bf-path (g/successors g) "P7X" "9H6")
  (task2)

  )
