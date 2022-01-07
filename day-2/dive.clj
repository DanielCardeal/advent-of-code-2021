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
(defn dir_displacement
  "Get the total displacement for a given direction."
  [dir dirs]
  (->> (filter #(= dir (first %)) dirs)
       (map second)
       (apply +)))

(defn tot_displacement
  "Calculate the total displacement of the submarine based on a list of
  directions."
  [dirs]
  (let [horiz (dir_displacement :forward dirs)
        depth (- (dir_displacement :down dirs)
                 (dir_displacement :up dirs))]
    {:horiz horiz, :depth depth}))

;; Resposta
(let [{h :horiz d :depth} (tot_displacement directions)]
  (* h d))
