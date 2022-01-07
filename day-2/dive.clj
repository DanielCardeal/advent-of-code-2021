(ns dive
  (:require [clojure.string :as str]))

;; --- Carregamento do input
(def INPUT_FILE "dive_input.txt")
(def directions
  "Input data for the puzzle."
  (let [lines (str/split-lines (slurp INPUT_FILE))
        ch->sym_table {\f :forward, \d :down, \u :up}
        str->dir #(get ch->sym_table (first %))]
    (->> lines
         (map #(str/split % #"\s+"))
         (map #(list (str->dir (first %))
                     (Integer/parseInt (second %)))))))

;; --- Parte 1
(defn tot_displacement
  "Calculate the total displacement of the submarine based on a list of
  directions."
  [dirs]
  (let [add-disp (fn [acc val]
                   (let [[horiz depth] acc
                         [dir amount] val]
                     (case dir
                       :forward (list (+ horiz amount) depth)
                       :up (list horiz (- depth amount))
                       :down (list horiz (+ depth amount)))))]
    (reduce add-disp [0 0] dirs)))

;; Resposta
(apply * (tot_displacement directions))

;; --- Parte 2
(defn tot_displacement'
  "Calculate the total displacement of the submarine based on a list of
  directions and the new found interpretation of each command."
  [dirs]
  (let [add-disp (fn [acc value]
                   (let [[horiz depth aim] acc
                         [dir amount] value]
                     (case dir
                       :up (list horiz depth (- aim amount))
                       :down (list horiz depth (+ aim amount))
                       :forward (list (+ horiz amount)
                                      (+ depth (* aim amount))
                                      aim))))
        [horiz depth _] (reduce add-disp [0 0 0] dirs)]
    {:horiz horiz, :depth depth}))

;; Resposta
(let [{h :horiz d :depth} (tot_displacement' directions)]
     (* h d))
