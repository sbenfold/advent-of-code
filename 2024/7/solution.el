;;; solution.el --- Advent of Code 2024 Day 7 solution  -*- lexical-binding: t -*-

(require 'ht)

(defconst aoc/7/input-file "input.txt")
;; (defconst aoc/7/input-file "example-input.txt")

(defun aoc/7/parse-line (raw-line)
  (->> raw-line
     (s-replace ":" "")
     (s-split " ")
     (-map #'string-to-number)
     ))

(defun aoc/7/-eval-line (acc operands operators)
  (if (null operators)
      acc
    (aoc/7/-eval-line
     (apply (car operators) (list acc (car operands)))
     (cdr operands)
     (cdr operators))))

(defun aoc/7/eval-line (operands operators)
  (aoc/7/-eval-line (car operands) (cdr operands) operators))

(defun aoc/7/solve-line (operators line)
  (-let (((goal . operands) line))
    (when (-contains?
           (->> (apply '-table-flat 'list (-repeat (1- (length operands)) operators))
              (--map (aoc/7/eval-line operands it))
              )
           goal)
      goal)
    ))

(defun aoc/7/solve (operators)
  (->> (f-read-text aoc/7/input-file)
     (s-lines)
     (-remove #'s-blank?)
     (-map #'aoc/7/parse-line)
     (--map (aoc/7/solve-line operators it))
     (-non-nil)
     (-sum)
     ))

;; 7a
(aoc/7/solve (list '* '+))

(defun aoc/7/concat (x y)
  (->> (list x y)
     (-map #'number-to-string)
     (-reduce #'concat)
     (string-to-number)
     ))

;; 7b
(aoc/7/solve (list '* '+ #'aoc/7/concat))
