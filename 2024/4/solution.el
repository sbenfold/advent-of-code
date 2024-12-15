;;; solution.el --- Advent of Code 2024 Day 4 solution  -*- lexical-binding: t -*-

(require 'generator)

;; (defconst aoc/4/input-file "input.txt")
(defconst aoc/4/input-file "example-input-a1.txt")
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

;; 4a
(let* ((raw-input (f-read-text aoc/4/input-file))
       (input (->> raw-input
                 (s-lines)
                 (-reduce #'s-concat)))
       (w (s-index-of "\n" raw-input))
       (h (/ (length input) w))
       )
  ;; raw-input
  ;; (aoc/4/iter-horiz input w h)
  ;; (aoc/4/iter-vert input w h)
  ;; (aoc/4/iter-diag-br input w h)
  ;; (aoc/4/get-diag-br input w h 0 0)
  ;; (aoc/4/get-diag-br input w h 0 1)
  ;; (aoc/4/get-diag-br input w h 1 0)
  ;; (aoc/4/get-diag-bl input w h 5 3)
  (aoc/4/iter-diag-bl input w h)
  )

;; TODO Generate the following:
;;   1) TODO Reach row
;;   2) TODO Reach row
;;   3) TODO Each DR diagonal
;;   4) TODO Each DL diagonal
;; TODO Iterate over each generator
;;   TODO Search forwards and backwards for every occurrence
;; TODO Sum

;; (s-match-strings-all "mul(\\([0-9]+\\),\\([0-9]+\\))" (aoc/3/read-input))
;; (-map #'aoc/3/helper)
;; (-sum)
