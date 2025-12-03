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

(defun aoc/2025/2/is-repeating (s len)
  (if (eq (length s) len)
      s
    (s-shared-start (s-left len s)
                    (aoc/2025/2/is-repeating (s-chop-left len s) len))))

(defun aoc/2025/2/is-match (x n)
  (let* ((s (number-to-string x)))
    (and (zerop (% (length s) n))
         (eq (length (aoc/2025/2/is-repeating s n))
             n))))

(defun aoc/2025/2/is-match2 (x)
  (let* ((s (number-to-string x)))
    (->>
     (number-sequence 1 (/ (length s) 2))
     (-any? (lambda (len) (aoc/2025/2/is-match x len)))
     )))

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

(aoc/2025/2a "input.txt")
(aoc/2025/2b "input.txt")
