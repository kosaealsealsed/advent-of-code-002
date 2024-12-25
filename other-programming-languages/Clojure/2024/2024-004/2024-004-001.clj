(ns word-search
  (:require [clojure.java.io :as io]))

(def file-path "/uploads/input.txt")
(def target-word "XMAS")

;; Read the file and store the grid as a vector of strings
(defn read-grid [file-path]
  (with-open [reader (io/reader file-path)]
    (doall (line-seq reader))))

(def grid (read-grid file-path))

;; Define the grid dimensions
(def rows (count grid))
(def cols (count (first grid)))

;; Define movement directions (right, down, diagonals, etc.)
(def directions
  [[0 1]   ; Right
   [1 0]   ; Down
   [1 1]   ; Diagonal-right-down
   [1 -1]  ; Diagonal-left-down
   [0 -1]  ; Left
   [-1 0]  ; Up
   [-1 -1] ; Diagonal-left-up
   [-1 1]]) ; Diagonal-right-up

;; Function to check if a word exists in a given direction
(defn check-word [x y dx dy]
  (let [word-length (count target-word)]
    (every? (fn [i]
              (let [nx (+ x (* i dx))
                    ny (+ y (* i dy))]
                (and (>= nx 0) (< nx rows)
                     (>= ny 0) (< ny cols)
                     (= (nth target-word i)
                        (get (nth grid nx) ny)))))
            (range word-length))))

;; Count all occurrences of the target word
(defn count-word-occurrences []
  (reduce
    (fn [total-count r]
      (reduce
        (fn [row-count c]
          (+ row-count
             (count
               (filter true?
                       (map (fn [[dx dy]]
                              (check-word r c dx dy))
                            directions)))))
        total-count
        (range cols)))
    0
    (range rows)))

;; Output the count
(println (format "Occurrences of %s: %d" target-word (count-word-occurrences)))
