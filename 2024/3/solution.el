;;; solution.el --- Advent of Code 2024 Day 3 solution  -*- lexical-binding: t -*-

(require 'generator)

(defconst aoc/3/input-file "input.txt")
;; (defconst aoc/3/input-file "example-input.txt")
;; (defconst aoc/3/input-file "example-input-b.txt")

(defun aoc/3/read-input ()
  (->> (f-read-text aoc/3/input-file)
     (s-trim)))

(defun aoc/3/helper (xs)
  ;; (message "xs: [[%s]]" xs)
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
;; Algo 1
;; TODO Parse "don't" and "do"
;; TODO Add a "do" to the start and "don't" to the end
;; TODO Find intervals between "do" and "don't"
;; TODO Only accept matches with these intervals

;; Algo 2
;; TODO While string contains "don't"
;;   TODO Find next "do" (not "don't") or EOF
;;   TODO Delete this region

;; Algo 3
;; TODO While true:
;;   TODO Return string up to "don't" or EOF
;;   TODO Find next "do" (not "don't")

;; Algo 4
;; TODO Insert "do" at start and "don't" at end
;; TODO Make a generator that returns text between "do" and "don't"
;; TODO Iterate over generator

(iter-defun aoc/3/instr-generator ()
  (let ((input (concat "do() " (aoc/3/read-input) "don't()"))
        (pos 0))
    (while (string-match "do()\\(.*?\\)don't()" input pos)
      ;; Filter out "do" that matches "don't" since we don't have negative lookahead
      ;; (if (s-starts-with? "n't" (match-string 1 input))
      ;;     (setq pos (+ pos 5))
      (message "%d -> %d (%s)" (match-beginning 1) (match-end 1) (match-string 1 input))
      (iter-yield (match-string 1 input))
      (setq pos (match-end 0)))
    ;; )
    )
  )

(defun aoc/3/solve-one (parsed-input)
  (->>
   parsed-input
   ;; (--map (s-match-strings-all "mul(\\([0-9]+\\),\\([0-9]+\\))" it))
   (-map #'aoc/3/helper)
   (-sum))
  )

;; 70156309 is too low
(->>
 (cl-loop for x iter-by (aoc/3/instr-generator) collect x)
 ;; (-map #'aoc/3/solve-one)
 ;; )
 (--map (s-match-strings-all "mul(\\([0-9]+\\),\\([0-9]+\\))" it))
 (-map #'aoc/3/solve-one)
 ;; (--map (aoc/3/helper it))
 ;; (-map #'aoc/3/helper)
 ;; (-map #'string-to-number)
 ;; (-product)
 (-sum)
 )
