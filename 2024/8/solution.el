;;; solution.el --- Advent of Code 2024 Day 8 solution  -*- lexical-binding: t -*-

(require 'ht)

;; (defconst aoc/8/input-file "input.txt")
(defconst aoc/8/input-file "example-input.txt")

(defun aoc/8/vec-add (a b)
  (-zip-with #'+ a b))

(defun aoc/8/vec-sub (a b)
  (-zip-with #'- a b))

(defun aoc/8/vec-ge? (a b)
  (and (>= (car a) (car b))
       (>= (cadr a) (cadr b))))

(defun aoc/8/vec-lt? (a b)
  (and (< (car a) (car b))
       (< (cadr a) (cadr b))))

(defun aoc/8/vec-in-bounds? (dim v)
  (and (aoc/8/vec-ge? v '(0 0))
       (aoc/8/vec-lt? v dim)))

(defun aoc/8/gen-nodes-from-pair (dim a1 a2)
  (let* ((delta (aoc/8/vec-sub a2 a1)))
    (list (aoc/8/vec-sub a1 delta)
          (aoc/8/vec-add a2 delta))
    ))

(defun aoc/8/gen-nodes (dim antennas gen-nodes-func)
  (->>
   (-powerset antennas)
   (--filter (= 2 (length it)))
   (--map (funcall gen-nodes-func dim (car it) (cadr it)))
   )
  )

(defun aoc/8/solve (gen-nodes-func)
  (let* ((raw-input (f-read-text aoc/8/input-file))
         (input (s-replace "\n" "" raw-input))
         (w (s-index-of "\n" raw-input))
         (h (/ (length input) w))
         (dim (list w h))
         (all-antennas (ht-create))
         )
    (-each-indexed (-map #'identity input)
      (lambda (index x)
        (when (not (= x ?.))
          (ht-set! all-antennas x (cons (list (% index w) (/ index w))
                                        (ht-get all-antennas x))))))
    (->>
     (ht-values all-antennas)
     (--map (aoc/8/gen-nodes dim it gen-nodes-func))
     (-flatten-n 2)
     (-distinct)
     (--filter (aoc/8/vec-in-bounds? dim it))
     (length)
     )
    )
  )

;; 8a
(aoc/8/solve #'aoc/8/gen-nodes-from-pair)

(defun aoc/8/trace (from delta dim)
  (when (aoc/8/vec-in-bounds? dim from)
    (cons from (aoc/8/trace (aoc/8/vec-add from delta) delta dim))))

(defun aoc/8/gen-nodes-from-pair-traced (dim a1 a2)
  (let* ((delta (aoc/8/vec-sub a2 a1)))
    (append (aoc/8/trace a1 (aoc/8/vec-sub '(0 0) delta) dim)
            (aoc/8/trace a2 delta dim))))

;; 8b
(aoc/8/solve #'aoc/8/gen-nodes-from-pair-traced)
