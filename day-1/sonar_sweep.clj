(ns sonar_sweep
  (:require [clojure.string :as str]))

;; --- Parte 1
(defn count_measurements
  "Count how many measurements are larger than their predecessor."
  ([ms] (count_measurements 0 (first ms) (rest ms)))
  ([tot prev ms]
   (if (empty? ms) tot
       (if (> (first ms) prev)
         (count_measurements (+ tot 1) (first ms) (rest ms))
         (count_measurements tot (first ms) (rest ms))))))

;; Tratamento do input
(def INPUT_FILE "sonar_sweep_input.txt")
(def measurements
  "Input data from the puzzle."
  (map #(Integer/parseInt %)
       (-> (slurp INPUT_FILE)
           (str/split #"\s+"))))

;; Resposta:
(count_measurements measurements)
