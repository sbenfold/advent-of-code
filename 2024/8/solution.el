;;; solution.el --- Advent of Code 2024 Day 8 solution  -*- lexical-binding: t -*-

(require 'ht)

;; (defconst aoc/8/input-file "input.txt")
(defconst aoc/8/input-file "example-input.txt")

(defun aoc/8/gen-nodes (w antennas)
  ;; TODO Enumerate all combinations of antinode positions
  ;;   Find delta = (B - A), then enumerate (A - delta), (B + delta)
  )

(defun aoc/8/solve ()
  (let* ((raw-input (f-read-text aoc/8/input-file))
         (input (s-replace "\n" "" raw-input))
         (w (s-index-of "\n" raw-input))
         (all-antennas (ht-create))
         )
    (-each-indexed (-map #'identity input)
      (lambda (index x)
        (when (not (= x ?.))
          (ht-set! all-antennas x (cons (cons (% index w) (/ index w))
                                        (ht-get all-antennas x))))))
    ;; TODO Call aoc/8/gen-nodes for each bucket in all-antennas
    all-antennas
    ;; TODO Filter out any outside grid
    ;; TODO Unique
    ;; TODO Sum
    )
  )

;; 8a
;; (aoc/8/solve)
