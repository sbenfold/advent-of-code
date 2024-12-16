;;; solution.el --- Advent of Code 2024 Day 6 solution  -*- lexical-binding: t -*-

(require 'ht)

;; (defconst aoc/6/input-file "input.txt")
(defconst aoc/6/input-file "example-input.txt")

(defun aoc/6/loc-to-index (w loc)
  (+ (car loc)
     (* (cdr loc) w)))

(defun aoc/6/try-loc (input w h current-loc current-dir)
  (let* ((test-loc (cons (+ (car current-loc) (car current-dir))
                         (+ (cdr current-loc) (cdr current-dir))))
         (test-index (aoc/6/loc-to-index w test-loc)))
    (message "%s -> %s" current-loc test-loc)
    (cond
     ((< (car test-loc) 0) :outside)
     ((>= (car test-loc) w) :outside)
     ((< (cdr test-loc) 0) :outside)
     ((>= (cdr test-loc) h) :outside)
     ((eq (aref input test-index) ?#) :blocked)
     (t :free)
     )
    ))

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
  ;; (while current-loc
  (message "Current loc: %s" current-loc)
  (aset input current-index ?X)
  ;; TODO This depends on dir
  ;; TODO Rotate 90 = (-y, x)

  ;; (aoc/6/try-loc input w h current-loc current-dir)
  (cl-ecase (aoc/6/try-loc input w h current-loc current-dir)
    ;; TODO Break out of iteration
    (:free "free")
    ;; TODO Rotate 90
    (:blocked "blocked")
    ;; TODO Stop algorithm
    (:outside "outside"))
  ;; (let ((new-loc nil))
  ;;   (while (null new-loc)
  ;;     (let ((test-loc (+ current-loc
  ;;                        (car current-dir)
  ;;                        (* (cdr current-dir) w))))
  ;;       (when (< test-loc 0)
  ;;         (setq test-loc nil))
  ;;       (while (eq? (aref input test-loc) ?#)
  ;;         (setq current-dir (cons (- (cdr current-dir)) (car current-dir))))
  ;;       ;; TODO Recalculate test-loc
  ;;       (setq current-loc test-loc))))
  ;; )
  ;; TODO Set current location to X
  ;; TODO Calculate next location
  ;;   TODO If next location is # then turn right
  ;;   TODO If next location is out of bounds then stop
  )
