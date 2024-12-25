#lang racket

;; Read the file and store the grid as a list of lists of characters
(define (read-grid file-path)
  (let ((in-port (open-input-file file-path)))
    (let loop ((lines '()))
      (let ((line (read-line in-port 'any)))
        (if (eof-object? line)
            (begin
              (close-input-port in-port)
              (reverse lines))
            (loop (cons (string->list line) lines)))))))

;; Count all X-MAS patterns in the grid
(define (count-all-xmas-patterns grid)
  (let* ((rows (length grid))
         (cols (length (car grid)))
         (count 0))
    ;; Helper function to get a cell from the grid
    (define (get-cell r c)
      (if (and (>= r 0) (< r rows) (>= c 0) (< c cols))
          (list-ref (list-ref grid r) c)
          #f))
    ;; Helper function to check for X-MAS pattern
    (define (check-pattern r c)
      (let* ((center (get-cell r c))
             (top-left (get-cell (- r 1) (- c 1)))
             (top-right (get-cell (- r 1) (+ c 1)))
             (bottom-left (get-cell (+ r 1) (- c 1)))
             (bottom-right (get-cell (+ r 1) (+ c 1))))
        (and (char=? center #\A)
             (or (and (char=? top-left #\M) (char=? top-right #\S)
                      (char=? bottom-left #\M) (char=? bottom-right #\S))
                 (and (char=? top-left #\S) (char=? top-right #\M)
                      (char=? bottom-left #\S) (char=? bottom-right #\M))
                 (and (char=? top-left #\M) (char=? top-right #\M)
                      (char=? bottom-left #\S) (char=? bottom-right #\S))
                 (and (char=? top-left #\S) (char=? top-right #\S)
                      (char=? bottom-left #\M) (char=? bottom-right #\M))))))
    ;; Traverse the grid to count patterns
    (for ([r (in-range 1 (- rows 1))])
      (for ([c (in-range 1 (- cols 1))])
        (when (check-pattern r c)
          (set! count (+ count 1)))))
    count))

;; Main logic
(define file-path "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-004\\input.txt") ; Update with your file path
(define grid (read-grid file-path))
(define total-xmas-patterns (count-all-xmas-patterns grid))

(printf "Total X-MAS patterns: ~a\n" total-xmas-patterns)
