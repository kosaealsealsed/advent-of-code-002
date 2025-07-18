#lang racket

(define target-word "XMAS")

;; Read the grid from stdin as a list of strings
(define (read-grid)
  (let loop ((lines '()))
    (let ((line (read-line)))
      (if (eof-object? line)
          (reverse lines)
          (loop (cons line lines))))))

(define grid (read-grid))

;; Define the grid dimensions
(define rows (length grid))
(define cols (string-length (car grid)))

;; Define movement directions (right, down, diagonals, etc.)
(define directions
  '((0 1)   ; Right
    (1 0)   ; Down
    (1 1)   ; Diagonal-right-down
    (1 -1)  ; Diagonal-left-down
    (0 -1)  ; Left
    (-1 0)  ; Up
    (-1 -1) ; Diagonal-left-up
    (-1 1))) ; Diagonal-right-up

;; Function to check if a word exists in a given direction
(define (check-word x y dx dy)
  (let loop ((i 0))
    (if (= i (string-length target-word))
        #t
        (let ((nx (+ x (* i dx)))
              (ny (+ y (* i dy))))
          (if (or (< nx 0) (>= nx rows) (< ny 0) (>= ny cols)
                  (not (char=? (string-ref target-word i)
                               (string-ref (list-ref grid nx) ny))))
              #f
              (loop (+ i 1)))))))

;; Count all occurrences of the target word
(define (count-word-occurrences)
  (let ((count 0))
    (for ([r (in-range rows)])
      (for ([c (in-range cols)])
        (for ([direction directions])
          (let* ([dx (first direction)]
                 [dy (second direction)])
            (when (check-word r c dx dy)
              (set! count (+ count 1)))))))
    count))

;; Output the count
(printf "Occurrences of ~a: ~a\n" target-word (count-word-occurrences))
