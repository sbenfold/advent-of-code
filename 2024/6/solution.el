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

(defun aoc/6/parse-input ()
  (let* ((raw-input (f-read-text aoc/6/input-file))
         (input (->> raw-input
                   (s-lines)
                   (-reduce #'s-concat)))
         (w (s-index-of "\n" raw-input)))
    (list input w (s-index-of "^" input))))

(defun aoc/6/mark-positions (input w start-index)
  (-let* ((h (/ (length input) w))
          (current-index start-index)
          (current-loc (cons (% current-index w) (/ current-index w)))
          (current-dir '(0 . -1))
          (seen-states (ht-create))
          (loop nil)
          )

    (while current-loc
      (aset input current-index ?X)
      (let ((hash (aoc/6b/hash-state current-index current-dir)))
        (if (ht-contains? seen-states hash)
            (progn
              (setq loop t)
              (setq current-loc nil))
          (ht-set! seen-states hash t)
          (let ((dir (aoc/6/calc-dir input w h current-loc current-dir)))
            (setq current-dir dir)
            (if (not current-dir)
                (setq current-loc nil)
              (setq current-loc (aoc/6/add-vecs current-loc current-dir))
              (setq current-index (aoc/6/loc-to-index w current-loc))
              ))))
      )
    (list input loop)
    ))
;; 6a
(s-count-matches "X" (car (apply #'aoc/6/mark-positions (aoc/6/parse-input))))

(defun aoc/6b/try-obstacle (board w start-index obstacle-index)
  (aset board obstacle-index ?#)
  ;; TODO Detect a loop by storing (position, direction). Loop if same (position, direction) found.
  ;; (aoc/6/mark-positions board w start-index)
  ;; (-let (((obstacle-output loop seen-states) (aoc/6/mark-positions (concat board) w start-index)))
  ;;   (message "%s\n%s\n%s\n%s\n" board obstacle-output loop seen-states))
  (cadr (aoc/6/mark-positions (concat board) w start-index))
  ;; nil
  )

(defun aoc/6b/hash-state (index dir)
  (+ (ash index 8)
     (ash (1+ (cdr dir)) 4)
     (1+ (car dir))))

;; 6b
(-let* (((initial-board w start-index) (aoc/6/parse-input))
        ((solved-board loop) (aoc/6/mark-positions (concat initial-board) w start-index)))
  (s-matched-positions-all "X" solved-board)
  (->> (s-matched-positions-all "X" solved-board)
     (mapcar #'car)
     (-remove-item start-index)
     (--map (aoc/6b/try-obstacle (concat initial-board) w start-index it))
     (-flatten)
     (length)
     )
  ;; (aoc/6b/try-obstacle (concat initial-board) w start-index 63)
  )
