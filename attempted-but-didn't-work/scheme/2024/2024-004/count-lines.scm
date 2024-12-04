(define (count-lines filename)
  (call-with-input-file filename
    (lambda (in-port)
      (let loop ((line-count 0))
        (let ((line (read-line in-port #f))) ; Use #f to specify no peek
          (if (eof-object? line)
              line-count
              (loop (+ line-count 1))))))))
              
;; Example usage:
(define filename "input.txt") ;; Replace with your file path
(display (string-append "Number of lines: " (number->string (count-lines filename)) "\n"))
