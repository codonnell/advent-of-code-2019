(ns advent-of-code-2019.day7
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [loom.graph :as g]
            [loom.alg-generic :as alg]))

(defn get-param-value [{:keys [instructions]} param parameter-mode]
  (case parameter-mode
    0 (nth instructions param)
    1 param))

(declare ops)

(defn get-params [{:keys [index instructions] :as program} {:keys [op parameter-modes]}]
  (mapv (fn [param-index]
          (cond->> (nth instructions (+ 1 index param-index))
            (and
              (= :input (get-in ops [op :param-spec param-index]))
              (= 0 (nth parameter-modes param-index)))
            (nth instructions)))
    (range (count (get-in ops [op :param-spec])))))

(def ops
  {1 {:param-count 3
      :param-spec [:input :input :output]
      :execute (fn [{:keys [instructions index] :as program} {:keys [parameter-modes]}]
                 (let [input1 (get-param-value program (nth instructions (inc index)) (first parameter-modes))
                       input2 (get-param-value program (nth instructions (+ 2 index)) (second parameter-modes))]
                   (-> program
                     (assoc-in [:instructions (nth instructions (+ 3 index))] (+ input1 input2))
                     (update :index + (inc (get-in ops [1 :param-count]))))))}
   2 {:param-count 3
      :param-spec [:input :input :output]
      :execute (fn [{:keys [instructions index] :as program} {:keys [parameter-modes]}]
                 (let [input1 (get-param-value program (nth instructions (inc index)) (first parameter-modes))
                       input2 (get-param-value program (nth instructions (+ 2 index)) (second parameter-modes))]
                   (-> program
                     (assoc-in [:instructions (nth instructions (+ 3 index))] (* input1 input2))
                     (update :index + (inc (get-in ops [2 :param-count]))))))}
   3 {:param-count 1
      :param-spec [:output]
      :execute (fn [{:keys [instructions inputs index] :as program} op-code]
                 (if (seq inputs)
                   (let [[input1] (get-params program op-code)]
                     (-> program
                       (assoc-in [:instructions input1] (first inputs))
                       (update :inputs #(subvec % 1))
                       (update :index + (inc (get-in ops [3 :param-count])))))
                   (assoc program :needs-input true)))}
   4 {:param-count 1
      :param-spec [:output]
      :execute (fn [{:keys [instructions index] :as program} {:keys [parameter-modes]}]
                 (-> program
                   (update :outputs (fnil conj [])
                     (get-param-value program (nth instructions (inc index)) (first parameter-modes)))
                   (update :index + (inc (get-in ops [4 :param-count])))))}
   5 {:param-count 2
      :param-spec [:input :input]
      :execute (fn [{:keys [instructions index] :as program} op-code]
                 (let [[input1 input2] (get-params program op-code)]
                   (assoc program :index
                     (if (not= 0 input1)
                       input2
                       (+ index (inc (get-in ops [5 :param-count])))))))}
   6 {:param-count 2
      :param-spec [:input :input]
      :execute (fn [{:keys [instructions index] :as program} op-code]
                 (let [[input1 input2] (get-params program op-code)]
                   (assoc program :index
                     (if (zero? input1)
                       input2
                       (+ index (inc (get-in ops [6 :param-count])))))))}
   7 {:param-count 3
      :param-spec [:input :input :output]
      :execute (fn [{:keys [instructions index] :as program} op-code]
                 (let [[input1 input2 input3] (get-params program op-code)]
                   (-> program
                     (assoc-in [:instructions input3] (if (< input1 input2) 1 0))
                     (update :index + (inc (get-in ops [7 :param-count]))))))}
   8 {:param-count 3
      :param-spec [:input :input :output]
      :execute (fn [{:keys [instructions index] :as program} op-code]
                 (let [[input1 input2 input3] (get-params program op-code)]
                   (-> program
                     (assoc-in [:instructions input3] (if (= input1 input2) 1 0))
                     (update :index + (inc (get-in ops [8 :param-count]))))))}
   99 {:param-count 0
       :execute (fn [program _]
                  (assoc program :halted true))}})

(defn get-digits [n]
  (let [raw-digits (->> n str (mapv (comp #(Integer/parseInt %) str)))]
    (cond->> raw-digits
      (= 1 (count raw-digits))
      (into [0]))))

(defn pad-parameter-modes [parameter-modes op]
  (into parameter-modes
    (repeat (max 0 (- (get-in ops [op :param-count]) (count parameter-modes)))
      0)))

(defn parse-op-code [op-code]
  (let [digits (get-digits op-code)
        num-digits (count digits)
        op (+
             (* 10 (peek (pop digits)))
             (peek digits))]
    {:op op
     :parameter-modes (-> digits pop pop reverse vec (pad-parameter-modes op))}))

(defn step [{:keys [index instructions] :as program}]
  (let [{:keys [op parameter-modes] :as op-code} (parse-op-code (nth instructions index))
        {:keys [execute param-count]} (get ops op)]
    (execute program op-code)))

(defn run-program [instructions inputs]
  (loop [program {:instructions instructions :inputs inputs :index 0}]
    (if (or (:halted program) (:needs-input program))
      program
      (recur (step program)))))

(defn read-input []
  (mapv #(Integer/parseInt %) (str/split (str/trim (slurp "day7input")) #",")))

(defn run-phase-setting-sequence [instructions phase-settings]
  (reduce (fn [thruster-signal phase-setting]
            (-> instructions
              (run-program [phase-setting thruster-signal])
              :outputs
              first))
    0 phase-settings))

(defn task1 []
  (let [instructions (read-input)]
    (->> (range 5)
      combo/permutations
      (mapv (juxt identity #(run-phase-setting-sequence instructions %)))
      (sort-by second))))

(defn run-program* [program]
  (loop [{:keys [halted needs-input] :as program} program]
    (if (or halted needs-input)
      program
      (recur (step program)))))

(defn run-feedback-loop [instructions phase-settings]
  (let [feedback-pairs (partition 2 1 (cycle (range 5)))
        amp->program (into {}
                       (map (fn [[amp program]]
                              [amp (update program :inputs conj (nth phase-settings amp))]))
                       (zipmap (range 5) (repeat {:instructions instructions :inputs [] :index 0})))]
    (reduce (fn [amp->program [output-amp input-amp]]
              (let [ret (-> amp->program
                          (assoc-in [output-amp :needs-input] false)
                          (update-in [input-amp :inputs] conj (peek (get-in amp->program [output-amp :outputs])))
                          (update input-amp run-program*))]
                (cond-> ret
                  (and (= input-amp 4) (get-in ret [input-amp :halted]))
                  reduced)))
      (-> amp->program
        (update-in [0 :inputs] conj 0)
        (update 0 run-program*))
      feedback-pairs)))

(defn task2 []
  (let [instructions (read-input)]
    (->> (range 5 10)
      combo/permutations
      (mapv (juxt identity #(-> (run-feedback-loop instructions %) (get-in [4 :outputs]) peek)))
      (sort-by second))))

(comment
  (task1)
  (run-phase-setting-sequence input [2 0 1 4 3])

  (def input (read-input))
  (run-feedback-loop input [2 0 1 4 3])
  (run-feedback-loop input [9 7 8 5 6])
  (task2)

  (reduce (fn [finished? n]
            (if (> n 100)
              (reduced n)
              false))
    false (range))

  (run-program input 7)
  {:index 0 :inputs [1] :instructions input}
  (step (step (step {:index 0 :inputs [8] :instructions input})))
  (get-in (step {:index 0 :inputs [1] :instructions input}) [:instructions 225])
  (get-params {:index 0 :inputs [1] :instructions input} (parse-op-code 3))
  (get-param-value {:index 0 :inputs [1] :instructions input} 225 1)
  (parse-op-code 3)
  (get-digits 3)
  (nth input 225)
  (task2)

  )
