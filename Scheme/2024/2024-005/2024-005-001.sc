#lang racket

(require racket/list
         racket/string
         racket/file
         racket/format
         racket/queue)

;; Helper function to print errors to stderr
(define (err msg)
  (fprintf (current-error-port) "~a\n" msg))

;; Reads entire file content as a string
(define (read-file-as-string path)
  (with-input-from-file path
    (lambda ()
      (let ((content (port->string (current-input-port))))
        (string-trim content)))))

;; Define string-blank? if not available
(define (string-blank? str)
  (regexp-match? #px"^\\s*$" str))

;; Parse rules section: returns a list of (list x y) pairs
(define (parse-rules rules-section)
  (for/list ([line (filter (lambda (l) (not (string-blank? l)))
                           (string-split rules-section "\n"))])
    (define parts (map string-trim (string-split line "|")))
    (cond
      [(not (= (length parts) 2))
       (err (format "Invalid rule format: ~a" line))
       #f] ; Use #f to represent invalid entries
      [else
       (define maybe-x (string->number (first parts)))
       (define maybe-y (string->number (second parts)))
       (cond
         [(and maybe-x maybe-y)
          (list (exact-integer? maybe-x) (exact-integer? maybe-y))]
         [else
          (err (format "Invalid numbers in rule: ~a" line))
          #f])])))

;; Filter out invalid rules
(define (filter-valid-rules rules)
  (filter (lambda (r) (and (list? r) (= (length r) 2))) rules))

;; Parse updates section: returns a list of (list of integers)
(define (parse-updates updates-section)
  (for/list ([line (filter (lambda (l) (not (string-blank? l)))
                           (string-split updates-section "\n"))])
    (define parts (map string-trim (string-split line ",")))
    (define maybe-update (for/list ([p parts])
                           (string->number p)))
    (if (and (not (empty? maybe-update))
             (andmap number? maybe-update))
        maybe-update
        (begin
          (err (format "Invalid number in update: ~a" line))
          #f)))) ; Use #f to represent invalid entries

;; Remove invalid updates
(define (filter-valid-updates updates)
  (filter (lambda (u) (and (list? u) (not (member #f u)))) updates))

;; Creates a hash table: element -> index
(define (make-index-map lst)
  (define h (make-hash))
  (for ([x (in-range (length lst))])
    (hash-set! h (list-ref lst x) x))
  h)

;; Checks if update is ordered according to rules
(define (is-update-ordered? update rules)
  (define index-map (make-index-map update))
  (for/and ([r (in-list rules)])
    (define x (first r))
    (define y (second r))
    (cond
      [(not (hash-has-key? index-map x)) #t] ;; If x isn't in update, no constraint
      [(not (hash-has-key? index-map y)) #t] ;; If y isn't in update, no constraint
      [else
       (< (hash-ref index-map x) (hash-ref index-map y))])))

;; Get the middle page from the update list
;; For even-sized lists, take the upper middle: index length/2 (integer division)
(define (get-middle-page update)
  (list-ref update (quotient (length update) 2)))

;; Identify correct updates and collect their middle pages
(define (identify-correct-updates updates rules)
  (define correct-updates '())
  (define middle-pages '())
  (for ([u (in-list updates)])
    (when (is-update-ordered? u rules)
      (set! correct-updates (cons u correct-updates))
      (set! middle-pages (cons (get-middle-page u) middle-pages))))
  (values (reverse correct-updates) (reverse middle-pages)))

;; Topological sort the update according to rules
;; If cycle detected or not possible, return empty list
(define (topological-sort-update update rules)
  (define nodes (remove-duplicates update))
  (define graph (make-hash))
  (define in-degree (make-hash))
  
  ;; Initialize graph and in-degree
  (for ([n (in-list nodes)])
    (hash-set! graph n '())
    (hash-set! in-degree n 0))

  ;; Build graph and in-degree from rules
  (for ([r (in-list rules)])
    (define x (first r))
    (define y (second r))
    (when (and (memv x nodes) (memv y nodes))
      (hash-set! graph x (cons y (hash-ref graph x)))
      (hash-set! in-degree y (add1 (hash-ref in-degree y)))))

  ;; Queue for nodes with in-degree 0
  (define q (make-queue))
  (for ([n (in-list nodes)])
    (when (= (hash-ref in-degree n) 0)
      (queue-enqueue! q n)))

  (define sorted '())
  
  (let loop ()
    (unless (queue-empty? q)
      (define current (queue-dequeue! q))
      (set! sorted (cons current sorted))
      (for ([neighbor (in-list (hash-ref graph current))])
        (hash-set! in-degree neighbor (sub1 (hash-ref in-degree neighbor)))
        (when (= (hash-ref in-degree neighbor) 0)
          (queue-enqueue! q neighbor)))
      (loop)))
  
  (set! sorted (reverse sorted))
  (if (= (length sorted) (length nodes))
      sorted
      '()))

;; Identify incorrect updates, correct them via topological sort, and collect their middle pages
(define (identify-incorrect-updates updates rules)
  (define incorrect-updates '())
  (define incorrect-middle-pages '())
  (for ([u (in-list updates)])
    (unless (is-update-ordered? u rules)
      (define corrected (topological-sort-update u rules))
      (if (empty? corrected)
          (err (format "Cycle detected or unable to sort update: ~a"
                       (string-join (map number->string u) ", ")))
          (begin
            (set! incorrect-updates (cons corrected incorrect-updates))
            (set! incorrect-middle-pages (cons (get-middle-page corrected) incorrect-middle-pages))))))
  (values (reverse incorrect-updates) (reverse incorrect-middle-pages)))

;; Entry point similar to Scala main
;; Adjust file path as needed
(define (main)
  (define filePath "\\\\vmware-host\\Shared Folders\\C\\advent-of-code-002\\input-files\\2024\\2024-005\\input.txt")

  (define content (with-handlers ([exn:fail? (lambda (e)
                                               (err (format "Error reading the file: ~a" (exn-message e)))
                                               (exit 1))])
                     (read-file-as-string filePath)))

  (define sections (string-split content "\n\n"))
  (when (not (= (length sections) 2))
    (err "Invalid input format. Expected two sections separated by two newlines.")
    (exit 1))

  (define rules-section (first sections))
  (define updates-section (second sections))

  (define raw-rules (parse-rules rules-section))
  (define rules (filter-valid-rules raw-rules))
  (define raw-updates (parse-updates updates-section))
  (define updates (filter-valid-updates raw-updates))

  ;; Identify correctly ordered updates and their middle pages
  (define-values (correct-updates middle-pages) (identify-correct-updates updates rules))
  (define sum-middle-pages (apply + (if (empty? middle-pages) '(0) middle-pages)))
  (displayln (format "Sum of middle pages for correctly ordered updates: ~a" sum-middle-pages))

  ;; Identify incorrectly ordered updates and correct them
  (define-values (incorrect-updates incorrect-middle-pages) (identify-incorrect-updates updates rules))
  (define sum-incorrect-middle-pages (apply + (if (empty? incorrect-middle-pages) '(0) incorrect-middle-pages)))
  (displayln (format "Sum of middle pages for corrected updates: ~a" sum-incorrect-middle-pages)))

;; Run main if executed as a script
(main)
