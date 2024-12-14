;;; solution.el --- Advent of Code 2024 Day 3 solution  -*- lexical-binding: t -*-

(require 'generator)

(defun aoc/3/read-input ()
  ;; (->> (f-read-text "example-input.txt")
  (->> (f-read-text "input.txt")
     (s-trim)))

(iter-defun aoc/3/expr-generator ()
  (let* ((input (aoc/3/read-input))
         (pos 0))
    (while (string-match "mul(\\([0-9]+\\),\\([0-9]+\\))" input pos)
      (iter-yield (-map #'string-to-number (list (match-string 1 input)
                                                 (match-string 2 input))))
      (setq pos (match-end 0)))))

;; 3a
(->>
 (cl-loop for x
          iter-by (aoc/3/expr-generator)
          collect x)
 (--map (apply #'* it))
 (-sum))
