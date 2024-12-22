;;; solution.el --- Advent of Code 2024 Day 9 solution  -*- lexical-binding: t -*-

;; (defconst aoc/9/input-file "input.txt")
(defconst aoc/9/input-file "example-input.txt")

(defun aoc/9/expand-disk-pair (index pair)
  (concat
   (s-repeat (car pair) (number-to-string index))
   (s-repeat (cadr pair) ".")
   ))

(defun aoc/9/to-block-map (disk-map)
  (->> (concat disk-map "0")
     (s-split "")
     (-remove #'s-blank?)
     (-map #'string-to-number)
     (-partition 2)
     (-map-indexed #'aoc/9/expand-disk-pair)
     )
  )

(defun aoc/9/solve ()
  (->> (f-read-text aoc/9/input-file)
     (s-trim)
     (aoc/9/to-block-map)
     (s-join "")
     ))

(aoc/9/solve)
