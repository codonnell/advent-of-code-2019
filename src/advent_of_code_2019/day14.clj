(ns advent-of-code-2019.day14
  (:require [loom.graph :as g]
            [loom.alg :as alg]
            [clojure.math.numeric-tower :as math]
            [clojure.string :as str]))

;; IDEA: Create a directed graph with resource dependencies. Should have one source (ORE) and one sink (FUEL).
;; Topologically sort it and substitute resources until we get to the amount of ORE needed

(defn read-resource [s]
  (let [[n res] (str/split (str/trim s) #"\s")]
    [res (Long/parseLong n)]))

(defn read-input []
  (->> "day14input"
    slurp
    str/split-lines
    (into {}
      (comp
        (map #(str/split % #"=>"))
        (map (fn [[input-s output-s]]
               [(read-resource output-s)
                (mapv read-resource (str/split input-s #","))]))
        (map (fn [[[output-res output-num] inputs]]
               [output-res {:output-num output-num
                            :inputs (into {} inputs)}]))))))

(defn create-graph [requirements]
  (->> requirements
    (into [] (mapcat (fn [[output {:keys [inputs]}]]
                       (mapv #(vector output %) (keys inputs)))))
    (apply g/digraph)))

(defn task1 []
  (let [requirements (read-input)
        processing-order (butlast (alg/topsort (create-graph requirements)))]
    (reduce (fn [desired-resources resource]
              (let [desired-num (get desired-resources resource)
                    shipment-count (math/ceil (/ desired-num (get-in requirements [resource :output-num])))]
                (reduce-kv (fn [desired-resources input-res input-num]
                             (update desired-resources input-res (fnil + 0) (* input-num shipment-count)))
                  desired-resources (get-in requirements [resource :inputs]))))
      {"FUEL" 1} processing-order)))

(defn required-ore [fuel-count]
  (let [requirements (read-input)
        processing-order (butlast (alg/topsort (create-graph requirements)))]
    (get (reduce (fn [desired-resources resource]
                   (let [desired-num (get desired-resources resource)
                         shipment-count (math/ceil (/ desired-num (get-in requirements [resource :output-num])))]
                     (reduce-kv (fn [desired-resources input-res input-num]
                                  (update desired-resources input-res (fnil + 0) (* input-num shipment-count)))
                       desired-resources (get-in requirements [resource :inputs]))))
           {"FUEL" fuel-count} processing-order)
      "ORE")))

(comment
  (alg/topsort (create-graph (read-input)))
  (task1)
  (quot 1000000000000 522031)
  (required-ore 1915595) ; 537096316975N
  (required-ore (int (* (/ 1000000000000 537096316975) 1915595)))
  (required-ore (inc (int (* (/ 1000000000000 537096316975) 1915595))))
  (required-ore (inc (inc (int (* (/ 1000000000000 537096316975) 1915595)))))
  (inc (int (* (/ 1000000000000 537096316975) 1915595)))
  )
