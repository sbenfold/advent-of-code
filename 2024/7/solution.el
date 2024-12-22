;;; solution.el --- Advent of Code 2024 Day 7 solution  -*- lexical-binding: t -*-

(require 'ht)

;; (defconst aoc/7/input-file "input.txt")
(defconst aoc/7/input-file "input-reduced.txt")
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
(defun aoc/7/concat-fast (x y)
  (string-to-number (concat (number-to-string x) (number-to-string y))))

(defun aoc/7/concat-fast2 (x y)
  (+ (* x
        (expt 10
              (length (number-to-string y))))
     y))

(aoc/7/concat-fast2 15 13)

(defun aoc/7/digits (x)
  (cond ((zerop (/ x 10)) 1)
        ((zerop (/ x 100)) 2)
        ((zerop (/ x 1000)) 3)
        ((zerop (/ x 10000)) 4)
        (t nil)))

(defun aoc/7/digits2 (x)
  (if (zerop x)
      0
    (+ 1 (aoc/7/digits2 (/ x 10)))))

(defun aoc/7/-enum-line (acc goal operands operators)
  (if (null operands)
      (if (eq acc goal)
          goal
        nil)
    (--map
     (aoc/7/-enum-line
      (apply it (list acc (car operands)))
      goal
      (cdr operands)
      operators)
     operators)
    ))

(mapcar (list '+ '*))
(->> (aoc/7/-enum-line 81 3267 (list 40 27) (list '* '+ #'aoc/7/concat-fast))
   (-flatten)
   )

(defun aoc/7/solve-line (operators line)
  (-let (((goal . operands) line))
    (->> (aoc/7/-enum-line (car operands)
                         goal
                         (cdr operands)
                         (list '* '+ #'aoc/7/concat-fast))
       (-flatten)
       (-first-item)
       )
    ))

;; 7b
(aoc/7/solve (list '* '+ #'aoc/7/concat))
(aoc/7/solve (list '* '+ #'aoc/7/concat-fast))
;; (aoc/7/solve-line (list '* '+ #'aoc/7/concat-fast) (list 313997964 683 8 1 65 50 7 581 6 2))

(require 'benchmark)
(benchmark-elapse (garbage-collect))
