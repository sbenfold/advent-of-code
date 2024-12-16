;;; solution.el --- Advent of Code 2024 Day 5 solution  -*- lexical-binding: t -*-

(require 'generator)
(require 'ht)

(defconst aoc/5/input-file "input.txt")
;; (defconst aoc/5/input-file "example-input.txt")

(defun aoc/5/validate-update (pages deps-ht)
  (-all? #'null
         (--map
          (-intersection
           (ht-get deps-ht (nth it pages))
           (-take it pages)
           )
          (-iota (1+ (length pages)) 1)
          )))

;; 5a
(-let* ((lines (s-lines (f-read-text aoc/5/input-file)))
        ((deps-raw . updates-raw) (-split-on "" lines))
        (deps (->> deps-raw
                 (-sort #'s-less?)
                 (--map (s-split "|" it))
                 (-map (-partial #'-map #'string-to-number))
                 (-partition-by #'car)
                 ))
        (updates (->> updates-raw
                    (car)
                    (--map (s-split "," it))
                    (-map (-partial #'-map #'string-to-number))
                    ))
        (deps-ht (ht-create))
        )
  (->> deps
     (--map
      (let ((unzipped (-unzip-lists it)))
        (ht-set! deps-ht (caar unzipped) (cadr unzipped))
        )
      ))
  (->> updates
     (--filter (aoc/5/validate-update it deps-ht))
     (--map (nth (/ (length it) 2) it))
     (-sum)
     )
  )
