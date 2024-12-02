;; 2a => 341
(defun aoc/2/safe? (a)
  (let ((diffs (-zip-with #'- (cdr a) a)))
    (and
     (apply #'= (-map #'cl-signum diffs))
     (-every (-partial #'>= 3) (-map #'abs diffs)))))

(->> (f-read-text "input.txt")
   (s-trim)
   (s-lines)
   (-map #'s-split-words)
   (-map (-partial #'-map #'string-to-number))
   (-map #'aoc/2/safe?)
   (-non-nil)
   (length)
   )

;; 2b =? !405
;; Not 64 either (405 - 341)
(defun aoc/2/dampened-safe? (a)
  (->> (--map-indexed (-remove-at it-index a) a)
     (-map #'aoc/2/safe?)
     (-non-nil)
     ;; (-first #'identity)
     ))

(->> (f-read-text "input.txt")
   (s-trim)
   (s-lines)
   (-map #'s-split-words)
   (-map (-partial #'-map #'string-to-number))
   (-map #'aoc/2/dampened-safe?)
   (-non-nil)
   (length)
   )

(apply #'some #'identity (list nil t t))

;; TODO Call safe for each element removed
(aoc/2/safe? (list 1 3 2 4 5))

(let ((a (list 1 3 2 4 5)))
  (->> (--map-indexed (-remove-at it-index a) a)
     (-map #'aoc/2/safe?)
     ;; (-non-nil)
     (-first #'identity)
     )
  )

(aoc/2/dampened-safe? (list 7 6 4 2 1))
(aoc/2/dampened-safe? (list 1 2 7 8 9))
(aoc/2/dampened-safe? (list 9 7 6 2 1))
(aoc/2/dampened-safe? (list 1 3 2 4 5))
(aoc/2/dampened-safe? (list 8 6 4 4 1))
(aoc/2/dampened-safe? (list 1 3 6 7 9))
