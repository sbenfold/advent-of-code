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

(defun aoc/7/solve-line (line)
  (-let (((goal . operands) line))
    (when (-contains?
           (->> (apply '-table-flat 'list (-repeat (1- (length operands)) (list '* '+)))
              (--map (aoc/7/eval-line operands it))
              )
           goal)
      goal)
    ))

(defun aoc/7/parse-input ()
  (->> (f-read-text aoc/7/input-file)
     (s-lines)
     (-remove #'s-blank?)
     (-map #'aoc/7/parse-line)
     (-map #'aoc/7/solve-line)
     (-non-nil)
     (-sum)
     ))

(aoc/7/parse-input)

;; (aoc/7/solve-line '(3267 81 40 27))

;; ;; TODO How to evaluate a line with these operators? Apply and recurse? Reduce?
;; (let ((input-len 3))
;;   (->> (apply '-table-flat 'list (-repeat (1- input-len) (list '* '+)))
;;      (--map (aoc/7/eval-line (list 81 40 27) it))
;;      ))

;; (apply '+ (list 0 81))

;; (let ((operators (list '+ '+))
;;       (operands (list 81 40 47))
;;       (acc 0))
;;   (apply (car operators) (list acc (car operands))))
