;;; solution.el --- Advent of Code 2024 Day 3 solution  -*- lexical-binding: t -*-

(require 'generator)

(defconst aoc/3/input-file "input.txt")
;; (defconst aoc/3/input-file "example-input.txt")
;; (defconst aoc/3/input-file "example-input-b.txt")
;; (defconst aoc/3/input-file "my-example-input-b.txt")
;; (defconst aoc/3/input-file "my-example-input-b2.txt")

(defun aoc/3/read-input ()
  (->> (f-read-text aoc/3/input-file)
     (s-lines)
     (-reduce #'s-concat)
     ))

(defun aoc/3/helper (xs)
  (->> xs
       (-drop 1)
       (-map #'string-to-number)
       (-product)
       ))

;; 3a
(->>
 (s-match-strings-all "mul(\\([0-9]+\\),\\([0-9]+\\))" (aoc/3/read-input))
 (-map #'aoc/3/helper)
 (-sum))

;; 3b
(iter-defun aoc/3/instr-generator ()
  (let ((input (concat "do() " (aoc/3/read-input) "don't()"))
        (pos 0))
    (while (string-match "do()\\(.*?\\)don't()" input pos)
      (iter-yield (match-string 1 input))
      (setq pos (match-end 0)))
    )
  )

(defun aoc/3/solve-one (parsed-input)
  (->>
   parsed-input
   (-map #'aoc/3/helper)
   (-sum))
  )

(->>
 (cl-loop for x iter-by (aoc/3/instr-generator) collect x)
 (--map (s-match-strings-all "mul(\\([0-9]+\\),\\([0-9]+\\))" it))
 (-map #'aoc/3/solve-one)
 (-sum)
 )
