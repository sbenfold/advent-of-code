;;; solution.el --- Advent of Code 2024 Day 5 solution  -*- lexical-binding: t -*-

(require 'ht)

;; (defconst aoc/5/input-file "input.txt")
(defconst aoc/5/input-file "example-input.txt")

(defun aoc/5/validate-update (pages deps-ht)
  (-all? #'null
         (--map
          (-intersection
           (ht-get deps-ht (nth it pages))
           (-take it pages))
          (-iota (length pages) 1))))

;; 5a
(-let* ((lines (s-lines (f-read-text aoc/5/input-file)))
        ((deps-raw . updates-raw) (-split-on "" lines))
        (deps (->> deps-raw
                 (-sort #'s-less?)
                 (--map (s-split "|" it))
                 (-map (-partial #'-map #'string-to-number))
                 (-partition-by #'car)))
        (updates (->> updates-raw
                    (car)
                    (--map (s-split "," it))
                    (-map (-partial #'-map #'string-to-number))))
        (deps-ht (ht-create)))
  (->> deps
     (--map
      (let ((unzipped (-unzip-lists it)))
        (ht-set! deps-ht (caar unzipped) (cadr unzipped)))))
  (->> updates
     (--filter (aoc/5/validate-update it deps-ht))
     (--map (nth (/ (length it) 2) it))
     (-sum)))

(defun aoc/5b/fix-update (pages deps-ht)
  (-if-let
      (error-index
       (--find-index
        (-intersection
         (ht-get deps-ht (nth it pages))
         (-take it pages))
        (-iota (length pages) 1)))
      (let ((intersection (-intersection
                           (ht-get deps-ht (nth (1+ error-index) pages))
                           (-take (1+ error-index) pages)
                           )))
        (let* ((old-index (-elem-index (car intersection) pages))
               (error-val (nth (1+ error-index) pages))
               (new-pages (-insert-at old-index error-val (-remove-at (1+ error-index) pages))))
          (aoc/5b/fix-update new-pages deps-ht)))
    pages))

;; 5b
(-let* ((lines (s-lines (f-read-text aoc/5/input-file)))
        ((deps-raw . updates-raw) (-split-on "" lines))
        (deps (->> deps-raw
                 (-sort #'s-less?)
                 (--map (s-split "|" it))
                 (-map (-partial #'-map #'string-to-number))
                 (-partition-by #'car)))
        (updates (->> updates-raw
                    (car)
                    (--map (s-split "," it))
                    (-map (-partial #'-map #'string-to-number))))
        (deps-ht (ht-create)))
  (->> deps
     (--map
      (let ((unzipped (-unzip-lists it)))
        (ht-set! deps-ht (caar unzipped) (cadr unzipped)))))
  (->> updates
     (--remove (aoc/5/validate-update it deps-ht))
     (--map (aoc/5b/fix-update it deps-ht))
     (--map (nth (/ (length it) 2) it))
     (-sum)))
