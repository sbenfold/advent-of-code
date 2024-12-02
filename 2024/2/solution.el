(require 'dash)
(require 'f)
(require 's)

;; 2a
(defun aoc/2/safe? (a)
  (let ((diffs (-zip-with #'- (cdr a) a)))
    (and
     (apply #'= (-map #'cl-signum diffs))
     (--every (<= (abs it) 3) diffs)
     (not (zerop (car diffs))))))

(defun aoc/2/solve (safe-func)
  (->> (f-read-text "input.txt")
     (s-trim)
     (s-lines)
     (-map #'s-split-words)
     (-map (-partial #'-map #'string-to-number))
     (-map safe-func)
     (-non-nil)
     (length)
     )
  )
(aoc/2/solve #'aoc/2/safe?)

;; 2b
(defun aoc/2/dampened-safe? (a)
  (->> (--map-indexed (-remove-at it-index a) a)
     (-map #'aoc/2/safe?)
     (-non-nil)
     ))
(aoc/2/solve #'aoc/2/dampened-safe?)
