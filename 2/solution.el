(->> (f-read-text "example-input.txt")
   (s-trim)
   (s-lines)
   (-map #'s-split-words)
   (-map (-partial #'-map #'string-to-number))
   (-map #'aoc/2/fails)
   (-count #'null)
   )

(defun aoc/2/diffs (vals)
  (when (cadr vals)
    (cons (- (cadr vals) (car vals))
          (aoc/2/diffs (cdr vals)))))

(defun aoc/2/passes (a)
  (let* ((diffs (-zip-with #'- (cdr a) a)))
    ;; Check all values equal
    (and
     (apply #'= (-map #'cl-signum diffs))
     ;; Check abs(diffs) don't exceed 3
     (-every (-partial #'>= 3) (-map #'abs diffs))
     )
    )
  )

(defun aoc/2/fails (a)
  (let* ((diffs (-zip-with #'- (cdr a) a)))
    ;; Check all values equal
    (or
     (not (apply #'= (-map #'cl-signum diffs)))
     ;; Check abs(diffs) don't exceed 3
     (-any (-partial #'< 3) (-map #'abs diffs))
     )
    )
  )

;; (let* ((a (list 7 6 4 2 1))
;;        ;; (diffs (aoc/2/diffs a))
;;        (diffs (-zip-with #'- (cdr a) a))
;;        )
;;   ;; Check all values equal
;;   (apply #'= (-map #'cl-signum diffs))
;;   ;; Check abs(diffs) don't exceed 3
;;   (-every (-partial #'>= 3) (-map #'abs diffs))
;;   )

;; (let* ((a (list 7 6 4 2 1))
;;        (diffs (aoc/2/diffs a)))
;;   ;; (-zip a (cdr a))
;;   (-zip-with #'- (cdr a) a)
;;   ;; (-map #'- (-zip a (cdr a)))
;;   )

;; (apply #'= (list -1 -1 -1 -1))
