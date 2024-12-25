(ns xmas-patterns
  (:require [clojure.java.io :as io]))

;; Function to read the grid from the file
(defn read-grid [file-path]
  (with-open [reader (io/reader file-path)]
    (mapv #(vec %) (line-seq reader)))) ;; Converts each line to a vector of characters

;; Helper functions for pattern detection
(defn get-cell [grid rows cols r c]
  (if (and (>= r 0) (< r rows) (>= c 0) (< c cols))
    (get (get grid r) c)
    nil))

(defn check-pattern [grid rows cols r c]
  (let [center (get-cell grid rows cols r c)
        top-left (get-cell grid rows cols (dec r) (dec c))
        top-right (get-cell grid rows cols (dec r) (inc c))
        bottom-left (get-cell grid rows cols (inc r) (dec c))
        bottom-right (get-cell grid rows cols (inc r) (inc c))]
    (and (= center \A)
         (or (and (= top-left \M) (= top-right \S)
                  (= bottom-left \M) (= bottom-right \S))
             (and (= top-left \S) (= top-right \M)
                  (= bottom-left \S) (= bottom-right \M))
             (and (= top-left \M) (= top-right \M)
                  (= bottom-left \S) (= bottom-right \S))
             (and (= top-left \S) (= top-right \S)
                  (= bottom-left \M) (= bottom-right \M))))))

;; Function to count all XMAS patterns
(defn count-all-xmas-patterns [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (reduce
      (fn [count r]
        (+ count
           (reduce
             (fn [inner-count c]
               (if (check-pattern grid rows cols r c)
                 (inc inner-count)
                 inner-count))
             0
             (range 1 (dec cols)))))
      0
      (range 1 (dec rows)))))

;; Main logic
(defn -main []
  (let [file-path "/uploads/input.txt"
        grid (read-grid file-path)
        total-xmas-patterns (count-all-xmas-patterns grid)]
    (println (format "Total X-MAS patterns: %d" total-xmas-patterns))))
(-main)
