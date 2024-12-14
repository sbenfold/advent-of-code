;;; solution.el --- Advent of Code 2024 Day 3 solution  -*- lexical-binding: t -*-

(require 'generator)

(defun aoc/3/read-input ()
  ;; (->> (f-read-text "example-input.txt")
  (->> (f-read-text "input.txt")
     (s-trim)))

;; 3a
(->>
 (s-match-strings-all "mul(\\([0-9]+\\),\\([0-9]+\\))" (aoc/3/read-input))
 (--map (-drop 1 it))
 (--map (-map #'string-to-number it))
 (--map (apply #'* it))
 (-sum)
 )
