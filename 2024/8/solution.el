;;; solution.el --- Advent of Code 2024 Day 8 solution  -*- lexical-binding: t -*-

(require 'ht)

;; (defconst aoc/8/input-file "input.txt")
(defconst aoc/8/input-file "example-input.txt")

(defun aoc/8/vec-add (a b)
  (-zip-with #'+ a b))

(defun aoc/8/vec-sub (a b)
  (-zip-with #'- a b))

(aoc/8/vec-add '(3 4) '(1 5))
(aoc/8/vec-sub '(3 4) '(1 5))

(defun aoc/8/gen-nodes-from-pair (w antennas)
  (let* ((a1 (car antennas))
         (a2 (cadr antennas))
         (delta (aoc/8/vec-sub a2 a1)))
    (list (aoc/8/vec-sub a1 delta)
          (aoc/8/vec-add a2 delta))
    ))

(defun aoc/8/gen-nodes (w antennas)
  (->>
   (-powerset antennas)
   (--filter (= 2 (length it)))
   (--map (aoc/8/gen-nodes-from-pair w it))
   )
  )

(defun aoc/8/is-in-bounds (w h v)
  (and (>= (car v) 0)
       (< (car v) w)
       (>= (cadr v) 0)
       (< (cadr v) h)))

(defun aoc/8/solve ()
  (let* ((raw-input (f-read-text aoc/8/input-file))
         (input (s-replace "\n" "" raw-input))
         (w (s-index-of "\n" raw-input))
         (h (/ (length input) w))
         (all-antennas (ht-create))
         )
    (-each-indexed (-map #'identity input)
      (lambda (index x)
        (when (not (= x ?.))
          (ht-set! all-antennas x (cons (list (% index w) (/ index w))
                                        (ht-get all-antennas x))))))
    (->>
     (ht-values all-antennas)
     (--map (aoc/8/gen-nodes w it))
     (-flatten-n 2)
     (-distinct)
     (--filter (aoc/8/is-in-bounds w h it))
     (length)
     )
    )
  )

;; 8a
;; (aoc/8/solve)
