(ns binary_diagnostic
  (:require [clojure.string :as str]))

;; --- Parte 1
(defn- count-bit-values
  "Count the number of zeros and ones that appears in a given bit for all values
  of a report."
  [report bit]
  (let [count1 (->> report
                    (filter #(bit-test % bit))
                    (count))
        count0 (- (count report) count1)]
    [count0 count1]))

(defn- most-common-bit
  "Return the most common value (0 or 1) of a bit in a report."
  [report bit]
  (let [[count0 count1] (count-bit-values report bit)]
    (if (> count0 count1) 0 1)))

(defn- gamma-rate
  "Calculates the gamma-rate of a given report."
  [report num-bits]
  (let [bits (range num-bits -1 -1)
        pushBit (fn [tot bit]
                  (+ bit (bit-shift-left tot 1)))]
    (->> bits
        (map #(most-common-bit report %))
        (reduce pushBit 0))))

(defn- epsilon-rate
  "Calculates the epsilon rate of a report based on its gama rate."
  [gamma num-bits]
  (let [mask (->> (range num-bits)
                  (map #(Math/pow 2 %))
                  (map #(.intValue %))
                  (apply +))]
    (bit-xor gamma mask)))

(defn power-consumption
  "Calculates the total power consumption of the submarine, given its report."
  [report num-bits]
  (let [gamma (gamma-rate report num-bits)
        epsilon (epsilon-rate gamma num-bits)]
    (* gamma epsilon)))

;; --- Teste
(def INPUT_TESTE [2r00100 2r11110 2r10110 2r10111 2r10101 2r01111 2r00111 2r11100 2r10000 2r11001 2r00010 2r01010])
(assert (= (power-consumption INPUT_TESTE 5) 198))

;; --- Tratamento do input
(def INPUT_FILE "binary_diagnostic_input.txt")
(let [lines (str/split-lines (slurp INPUT_FILE))
      report (map #(Integer/parseInt % 2) lines)
      num-bits (count (first lines))]
  (power-consumption report num-bits))
