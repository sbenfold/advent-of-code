;;; solution.el --- Advent of Code 2024 Day 5 solution  -*- lexical-binding: t -*-

(require 'generator)
(require 'ht)

;; (defconst aoc/5/input-file "input.txt")
(defconst aoc/5/input-file "example-input.txt")

(defun aoc/5/read-input ()
  (-let* ((lines (s-lines (f-read-text aoc/5/input-file)))
          ((deps-raw . pages-raw) (-split-on "" lines))
          (deps (->> deps-raw
                   (-sort #'s-less?)
                   (--map (s-split "|" it))
                   (-map (-partial #'-map #'string-to-number))
                   (-partition-by #'car)
                   ))
          (pages (->> pages-raw
                    (car)
                    (--map (s-split "," it))
                    (-map (-partial #'-map #'string-to-number))
                    ))
          (deps-ht (ht-create))
          )
    (->> deps
       (--map
        (let ((unzipped (-unzip it)))
          (ht-set! deps-ht (caar unzipped) (cadr unzipped))
          )
        ))
    ;; (ht->plist deps-ht)
    ;; updates
    ))

;; 5a
;; TODO Build a DAG from each dep pair. DAG is page -> dependencies
;; TODO Filter out incorrectly ordered updates
;;   TODO Iterate through pages and look up deps. Build a set of available deps while iterating.
;; TODO Get middle page number
;; TODO Sum
