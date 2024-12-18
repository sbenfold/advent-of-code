;;; solution.el --- Advent of Code 2024 Day 4 solution  -*- lexical-binding: t -*-

(require 'generator)

(defconst aoc/4/input-file "input.txt")
;; (defconst aoc/4/input-file "example-input-a1.txt")
;; (defconst aoc/4/input-file "example-input-a2.txt")

(defun aoc/4/read-input ()
  (->> (f-read-text aoc/4/input-file)
     (s-lines)
     (-reduce #'s-concat)
     ))

(defun aoc/4/iter-horiz (input w h)
  (--map (substring input (* it w) (* (1+ it) w))
        (-iota h)))

(defun aoc/4/get-column (input w h col-index)
  (apply #'string
         (--map (aref input (+ (* it w) col-index))
               (-iota h))
         ))

(defun aoc/4/iter-vert (input w h)
  (--map (aoc/4/get-column input w h it)
        (-iota w)))

(defun aoc/4/get-diag-br (input w h x y)
  (let ((n (min (- w x)
                (- h y))))
    (apply #'string (--map (aref input (+ (* w (+ it y)) it x))
                           (-iota n)
                           ))
    ))

(defun aoc/4/get-diag-bl (input w h x y)
  (let ((n (min (1+ x) (- h y))))
    (apply #'string (--map (aref input (+ (* w (+ it y)) (- it) x))
                           (-iota n)
                           ))
    ))

(defun aoc/4/iter-diag-br (input w h)
  (append
   (--map (aoc/4/get-diag-br input w h it 0)
         (-iota w))
   (--map (aoc/4/get-diag-br input w h 0 it)
         (-iota h 1))
   )
  )

(defun aoc/4/iter-diag-bl (input w h)
  (append
   (--map (aoc/4/get-diag-bl input w h it 0)
         (-iota w))
   (--map (aoc/4/get-diag-bl input w h (- w 1) it)
         (-iota h 1))
   )
  )

(defun aoc/4/iter-all (input w h)
  (append
   (aoc/4/iter-horiz input w h)
   (aoc/4/iter-vert input w h)
   (aoc/4/iter-diag-br input w h)
   (aoc/4/iter-diag-bl input w h)
   )
  )

(defun aoc/4/count (inputs)
  (->>
   inputs
   (--map (s-match-strings-all "XMAS\\|SAMX" it))
   (-non-nil)
   (-flatten)
   (length)
   )
  )

;; 4a
(let* ((raw-input (f-read-text aoc/4/input-file))
       (input (->> raw-input
                 (s-lines)
                 (-reduce #'s-concat)))
       (w (s-index-of "\n" raw-input))
       (h (/ (length input) w))
       )
  (aoc/4/count
   (aoc/4/iter-all input w h)
   )
  )

(defun aoc/4b/is-mas (a b)
  (or
   (and (eq a ?M) (eq b ?S))
   (and (eq b ?M) (eq a ?S))
   )
  )

(defun aoc/4b/try-pos (input w x y)
  (and
   (eq (aref input (+ (* y w) x)) ?A)
   (aoc/4b/is-mas
    (aref input (+ (* (1- y) w) (1- x)))
    (aref input (+ (* (1+ y) w) (1+ x)))
    )
   (aoc/4b/is-mas
    (aref input (+ (* (1- y) w) (1+ x)))
    (aref input (+ (* (1+ y) w) (1- x)))
    )
   )
  )

(defun aoc/4b/iter-row (input w y)
  (--map (aoc/4b/try-pos input w it y) (-iota (- w 2) 1))
  )

;; 4b
(let* ((raw-input (f-read-text aoc/4/input-file))
       (input (->> raw-input
                 (s-lines)
                 (-reduce #'s-concat)))
       (w (s-index-of "\n" raw-input))
       (h (/ (length input) w))
       )
  (->> (-iota (- h 2) 1)
     (--map (aoc/4b/iter-row input w it))
     (-flatten)
     (length)
     ))
