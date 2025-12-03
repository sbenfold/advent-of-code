(require 'dash)
(require 'f)
(require 's)

;; 44487518055
(defun aoc/2025/2a (filename)
  (->> filename
       (f-read)
       (s-trim)
       (s-split ",")
       (--map (apply 'number-sequence (->> it
                                           (s-split "-")
                                           (-map 'string-to-number))))
       (-flatten)
       (-filter (lambda (x)
                  (let* ((s (number-to-string x))
                         (mid (/ (length s) 2)))
                    (and (evenp (length s))
                         (equal (s-left mid s)
                                (s-right mid s))))))
       (-sum)))

(defun aoc/2025/2/is-repeating (s sublen)
  (when (zerop (% (length s) sublen))
    (let* ((seq (number-sequence 0 (length s) sublen))
           (pairs (-zip-pair seq (cdr seq))))
      (eq
       (->> pairs
            (--map (funcall 'substring s (car it) (cdr it)))
            (-reduce 's-shared-start)
            (length)
            ) sublen))))


(defun aoc/2025/2/is-match (x n)
  (let* ((s (number-to-string x)))
    (aoc/2025/2/is-repeating s n)))

(defun aoc/2025/2/is-match2 (x)
  (let* ((s (number-to-string x)))
    (->>
     (number-sequence 1 (/ (length s) 2))
     (-any? (-partial 'aoc/2025/2/is-repeating s))))
  )

;; 53481866137
(defun aoc/2025/2b (filename)
  (->> filename
       (f-read)
       (s-trim)
       (s-split ",")
       (--map (apply 'number-sequence (->> it
                                           (s-split "-")
                                           (-map 'string-to-number))))
       (-flatten)
       (-filter 'aoc/2025/2/is-match2)
       (-sum)
       ))

(aoc/2025/2a "example.txt")
(aoc/2025/2b "example.txt")
