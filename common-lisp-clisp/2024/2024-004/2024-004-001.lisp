(defparameter *file-path* "/uploads/input.txt")
(defparameter *target-word* "XMAS")

;; Read the file and store the grid as a list of strings
(defun read-grid (file-path)
  (with-open-file (stream file-path :direction :input)
    (loop for line = (read-line stream nil)
          until (null line)
          collect line)))

(defparameter *grid* (read-grid *file-path*))

;; Define the grid dimensions
(defparameter *rows* (length *grid*))
(defparameter *cols* (length (first *grid*)))

;; Define movement directions (right, down, diagonals, etc.)
(defparameter *directions*
  '((0 1)   ; Right
    (1 0)   ; Down
    (1 1)   ; Diagonal-right-down
    (1 -1)  ; Diagonal-left-down
    (0 -1)  ; Left
    (-1 0)  ; Up
    (-1 -1) ; Diagonal-left-up
    (-1 1))) ; Diagonal-right-up

;; Function to check if a word exists in a given direction
(defun check-word (x y dx dy)
  (loop for i from 0 below (length *target-word*)
        for nx = (+ x (* i dx))
        for ny = (+ y (* i dy))
        always (and (>= nx 0) (< nx *rows*)
                    (>= ny 0) (< ny *cols*)
                    (char= (char *target-word* i)
                           (char (nth nx *grid*) ny)))))

;; Count all occurrences of the target word
(defun count-word-occurrences ()
  (let ((count 0))
    (loop for r from 0 below *rows*
          do (loop for c from 0 below *cols*
                   do (loop for direction in *directions*
                            for dx = (first direction)
                            for dy = (second direction)
                            do (when (check-word r c dx dy)
                                 (incf count)))))
    count))

;; Output the count
(format t "Occurrences of ~A: ~A~%" *target-word* (count-word-occurrences))
