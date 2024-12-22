;;; solution.el --- Advent of Code 2024 Day 7 solution  -*- lexical-binding: t -*-

(defconst aoc/7/input-file "input.txt")
;; (defconst aoc/7/input-file "input-reduced.txt")
;; (defconst aoc/7/input-file "example-input.txt")

(defun aoc/7/parse-line (raw-line)
  (->> raw-line
     (s-replace ":" "")
     (s-split " ")
     (-map #'string-to-number)
     ))

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

(defun aoc/7/solve-line (operators line)
  (-let (((goal . operands) line))
    (->> (aoc/7/-enum-line (car operands)
                         goal
                         (cdr operands)
                         operators)
       (-flatten)
       (-first-item)
       )
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
;; (aoc/7/solve (list '* '+))

(defun aoc/7/concat (x y)
  (->> (list x y)
     (-map #'number-to-string)
     (-reduce #'concat)
     (string-to-number)
     ))

;; 7b
;; (aoc/7/solve (list '* '+ #'aoc/7/concat))
