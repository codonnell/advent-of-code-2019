(ns advent-of-code-2019.day13
  (:require [clojure.string :as str]))

(declare ops)

(defn get-params [{:keys [index instructions relative-base] :as program} {:keys [op parameter-modes]}]
  (mapv (fn [param-index]
          (let [instruction-value (get instructions (+' 1 index param-index) 0)
                parameter-mode (nth parameter-modes param-index)]
            (case [(get-in ops [op :param-spec param-index]) parameter-mode]
              [:input 0] (get instructions instruction-value 0)
              ([:output 0] [:input 1] [:output 1]) instruction-value
              [:input 2] (get instructions (+' relative-base instruction-value) 0)
              [:output 2] (+' relative-base instruction-value))))
    (range (count (get-in ops [op :param-spec])))))

(def ops
  {1 {:param-count 3
      :param-spec [:input :input :output]
      :execute (fn [{:keys [instructions index] :as program} op-code]
                 (let [[input1 input2 input3] (get-params program op-code)]
                   (-> program
                     (assoc-in [:instructions input3] (+' input1 input2))
                     (update :index +' (inc (get-in ops [1 :param-count]))))))}
   2 {:param-count 3
      :param-spec [:input :input :output]
      :execute (fn [{:keys [instructions index] :as program} op-code]
                 (let [[input1 input2 input3] (get-params program op-code)]
                   (-> program
                     (assoc-in [:instructions input3] (*' input1 input2))
                     (update :index +' (inc (get-in ops [2 :param-count]))))))}
   3 {:param-count 1
      :param-spec [:output]
      :execute (fn [{:keys [instructions inputs index] :as program} op-code]
                 (if (seq inputs)
                   (let [[input1] (get-params program op-code)]
                     (-> program
                       (assoc-in [:instructions input1] (first inputs))
                       (update :inputs #(subvec % 1))
                       (update :index +' (inc (get-in ops [3 :param-count])))))
                   (assoc program :needs-input true)))}
   4 {:param-count 1
      :param-spec [:input]
      :execute (fn [{:keys [instructions index] :as program} op-code]
                 (let [[input1] (get-params program op-code)]
                   (-> program
                     (update :outputs (fnil conj []) input1)
                     (update :index +' (inc (get-in ops [4 :param-count]))))))}
   5 {:param-count 2
      :param-spec [:input :input]
      :execute (fn [{:keys [instructions index] :as program} op-code]
                 (let [[input1 input2] (get-params program op-code)]
                   (assoc program :index
                     (if (not= 0 input1)
                       input2
                       (+' index (inc (get-in ops [5 :param-count])))))))}
   6 {:param-count 2
      :param-spec [:input :input]
      :execute (fn [{:keys [instructions index] :as program} op-code]
                 (let [[input1 input2] (get-params program op-code)]
                   (assoc program :index
                     (if (zero? input1)
                       input2
                       (+' index (inc (get-in ops [6 :param-count])))))))}
   7 {:param-count 3
      :param-spec [:input :input :output]
      :execute (fn [{:keys [instructions index] :as program} op-code]
                 (let [[input1 input2 input3] (get-params program op-code)]
                   (-> program
                     (assoc-in [:instructions input3] (if (< input1 input2) 1 0))
                     (update :index +' (inc (get-in ops [7 :param-count]))))))}
   8 {:param-count 3
      :param-spec [:input :input :output]
      :execute (fn [{:keys [instructions index] :as program} op-code]
                 (let [[input1 input2 input3] (get-params program op-code)]
                   (-> program
                     (assoc-in [:instructions input3] (if (= input1 input2) 1 0))
                     (update :index +' (inc (get-in ops [8 :param-count]))))))}
   9 {:param-count 1
      :param-spec [:input]
      :execute (fn [{:keys [instructions index] :as program} op-code]
                 (let [[input1] (get-params program op-code)]
                   (-> program
                     (update :relative-base +' input1)
                     (update :index +' (inc (get-in ops [9 :param-count]))))))}
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
        op (+'
             (*' 10 (peek (pop digits)))
             (peek digits))]
    {:op op
     :parameter-modes (-> digits pop pop reverse vec (pad-parameter-modes op))}))

(defn read-input []
  (into {}
    (map-indexed (fn [idx s] [idx (Long/parseLong s)]))
    (str/split (str/trim (slurp "day13input")) #",")))

(defn step [{:keys [index instructions] :as program}]
  (let [{:keys [op parameter-modes] :as op-code} (parse-op-code (get instructions index 0))
        {:keys [execute param-count]} (get ops op)]
    (execute program op-code)))

(defn run-program [program]
  (loop [{:keys [halted needs-input] :as program} program]
    (if (or halted needs-input)
      program
      (recur (step program)))))

(defn initial-state
  ([instructions]
   (initial-state instructions []))
  ([instructions inputs]
   {:instructions instructions :index 0 :relative-base 0 :inputs inputs}))

(def tile-id->object
  {0 :empty
   1 :wall
   2 :block
   3 :horizontal-paddle
   4 :ball})

(defn draw-object [board [x y tile-id]]
  (let [object (tile-id->object tile-id)]
    (cond-> (assoc board [x y] object)
      (= :ball object)
      (assoc :ball-position [x y])
      (= :horizontal-paddle object)
      (assoc :paddle-position [x y]))))

(defn task1 []
  (let [outputs (-> (read-input) initial-state run-program :outputs)
        board (transduce (partition-all 3) (completing draw-object) {} outputs)]
    (transduce
      (filter (fn [[_ object]] (= :block object)))
      (completing (fn [n _] (inc n)))
      0
      board)))

(defn play-game [program]
  (loop [program (assoc-in program [:instructions 0] 2)
         board {}]
    ))

(comment
  (task1)
  )
