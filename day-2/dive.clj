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
