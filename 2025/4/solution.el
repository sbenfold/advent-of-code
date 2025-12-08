(require 'dash)
(require 'f)
(require 's)

(defun aoc/2025/4/read-board (filename)
  (let* ((lines (->> filename (f-read) (s-trim) (s-lines)))
         (dim (length (car lines)))
         (data (->> lines
                    (--map (s-wrap it "."))
                    (s-join "")))
         (data-padded (s-wrap data
                              (s-repeat (+ dim 2) ".")))
         (initial-num (length (s-matched-positions-all "@" data))))
    (list data-padded dim initial-num)))

(defun aoc/2025/4/get-adjacents (data pos dim)
  (->> (-table-flat '+ (-iota 3 (- dim) dim) (-iota 3 (- pos 1)))
       (--map (eq (aref data it) ?@))
       (-count 'identity)))

(defun aoc/2025/4/get-removable (data-padded dim)
  (->> (s-matched-positions-all "@" data-padded)
       (-map 'car)
       (--annotate (aoc/2025/4/get-adjacents data-padded it (+ dim 2)))
       (--filter (< (car it) 5))
       (-map 'cdr)))

(defun aoc/2025/4/a (filename)
  (-let* (((data-padded dim initial-num) (aoc/2025/4/read-board filename)))
    (length (aoc/2025/4/get-removable data-padded dim))))

(defun aoc/2025/4/b-remove-iteration (data-padded dim)
  (let ((removable (aoc/2025/4/get-removable data-padded dim)))
    (if (null removable)
        data-padded
      (--each removable (aset data-padded it ?.))
      (aoc/2025/4/b-remove-iteration data-padded dim))))

(defun aoc/2025/4/b (filename)
  (-let* (((data-padded dim initial-num) (aoc/2025/4/read-board filename))
          (final-num (->> (aoc/2025/4/b-remove-iteration data-padded dim)
                          (s-matched-positions-all "@")
                          (length))))
    (- initial-num final-num)))

(aoc/2025/4/a "input.txt")
(aoc/2025/4/b "input.txt")
