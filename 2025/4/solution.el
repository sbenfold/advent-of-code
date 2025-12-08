(require 'dash)
(require 'f)
(require 's)

(defun aoc/2025/4/read-board (filename)
  (let* ((lines (->> filename (f-read) (s-trim) (s-lines)))
         (dim (length (car lines)))
         (data (-as-> lines data
                      (--map (s-wrap it ".") data)
                      (s-join "" data)
                      (s-wrap data (s-repeat (+ dim 2) "."))))
         (initial-num (s-count-matches "@" data)))
    (list data dim initial-num)))

(defun aoc/2025/4/get-adjacents (data pos dim)
  (->> (-table-flat '+ (-iota 3 (- dim) dim) (-iota 3 (- pos 1)))
       (--map (eq (aref data it) ?@))
       (-count 'identity)))

(defun aoc/2025/4/get-removable (data dim)
  (->> (s-matched-positions-all "@" data)
       (-map 'car)
       (--annotate (aoc/2025/4/get-adjacents data it (+ dim 2)))
       (--filter (< (car it) 5))
       (-map 'cdr)))

(defun aoc/2025/4/a (filename)
  (-let* (((data dim initial-num) (aoc/2025/4/read-board filename)))
    (length (aoc/2025/4/get-removable data dim))))

(defun aoc/2025/4/b-remove-iteration (data dim)
  (let ((removable (aoc/2025/4/get-removable data dim)))
    (if (null removable)
        data
      (--each removable (aset data it ?.))
      (aoc/2025/4/b-remove-iteration data dim))))

(defun aoc/2025/4/b (filename)
  (-let* (((data dim initial-num) (aoc/2025/4/read-board filename))
          (final-num (->> (aoc/2025/4/b-remove-iteration data dim)
                          (s-count-matches "@"))))
    (- initial-num final-num)))

(aoc/2025/4/a "input.txt")
(aoc/2025/4/b "input.txt")
