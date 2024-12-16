;;; solution.el --- Advent of Code 2024 Day 6 solution  -*- lexical-binding: t -*-

(require 'ht)

;; (defconst aoc/6/input-file "input.txt")
(defconst aoc/6/input-file "example-input.txt")

(defun aoc/6/loc-to-index (w loc)
  (+ (car loc)
     (* (cdr loc) w)))

(defun aoc/6/turn-right (dir)
  (cons (- (cdr dir)) (car dir)))

(defun aoc/6/add-vecs (v1 v2)
  (cons (+ (car v1) (car v2))
        (+ (cdr v1) (cdr v2))))

(defun aoc/6/try-loc (input w h current-loc current-dir)
  (let* ((test-loc (aoc/6/add-vecs current-loc current-dir))
         (test-index (aoc/6/loc-to-index w test-loc)))
    (cond
     ((< (car test-loc) 0) :outside)
     ((>= (car test-loc) w) :outside)
     ((< (cdr test-loc) 0) :outside)
     ((>= (cdr test-loc) h) :outside)
     ((eq (aref input test-index) ?#) :blocked)
     (t :free)
     )
    ))

(defun aoc/6/calc-dir (input w h current-loc dir)
  (cl-ecase (aoc/6/try-loc input w h current-loc dir)
    (:free dir)
    (:blocked (aoc/6/calc-dir input w h current-loc (aoc/6/turn-right dir)))
    (:outside nil)))

;; 6a
(let* ((raw-input (f-read-text aoc/6/input-file))
       (input (->> raw-input
                 (s-lines)
                 (-reduce #'s-concat)))
       (w (s-index-of "\n" raw-input))
       (h (/ (length input) w))
       (current-index (s-index-of "^" input))
       (current-loc (cons (% current-index w) (/ current-index w)))
       (current-dir '(0 . -1))
       )

  (while current-loc
    ;; (message "Current loc: %s" current-loc)
    (aset input current-index ?X)
    (let ((dir (aoc/6/calc-dir input w h current-loc current-dir)))
      (setq current-dir dir)
      (if (not current-dir)
          (setq current-loc nil)
        (setq current-loc (aoc/6/add-vecs current-loc current-dir))
        (setq current-index (aoc/6/loc-to-index w current-loc))
        )))
  (s-count-matches "X" input)
  )
