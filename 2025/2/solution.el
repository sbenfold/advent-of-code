(require 'dash)
(require 'f)
(require 's)

(defun aoc/2025/2/is-repeating (s sublen)
  (let* ((seq (number-sequence 0 (length s) sublen))
         (pairs (-zip-pair seq (cdr seq))))
    (eq
     (->> pairs
          (--map (funcall 'substring s (car it) (cdr it)))
          (-reduce 's-shared-start)
          (length)
          ) sublen)))

(defun aoc/2025/2/solve (filename &optional max-segments)
  (->> filename (f-read) (s-trim) (s-split ",")
       (--map (apply 'number-sequence (->> it
                                           (s-split "-")
                                           (-map 'string-to-number))))
       (-flatten)
       (-filter (lambda (x)
                  (let* ((s (number-to-string x)))
                    (->>
                     (number-sequence 2 (or max-segments (length s)))
                     (--filter (zerop (% (length s) it)))
                     (--map (/ (length s) it))
                     (-any? (-partial 'aoc/2025/2/is-repeating s))))
                  ))
       (-sum)))

(defun aoc/2025/2a (filename)
  (aoc/2025/2/solve filename 2))

(defun aoc/2025/2b (filename)
  (aoc/2025/2/solve filename))

;; (aoc/2025/2a "input.txt")
;; (aoc/2025/2b "input.txt")
