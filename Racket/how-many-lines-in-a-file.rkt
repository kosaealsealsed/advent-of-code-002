#lang racket

;; Function to count lines in a file
(define (count-lines file-path)
  (with-input-from-file file-path
    (lambda ()
      (let loop ((count 0))
        (let ((line (read-line)))
          (if (eof-object? line)
              count
              (loop (+ count 1))))))))

;; Main logic
(define file-path "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-004\\input.txt") ; Adjust your path here
(define num-lines (count-lines file-path))

(printf "The file ~a contains ~a lines.\n" file-path num-lines)
