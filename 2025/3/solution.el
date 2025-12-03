(require 'dash)
(require 'f)
(require 's)

(defun aoc/2025/3/max-by-first-digit (line first-digit)
  (let ((first-index (s-index-of first-digit line)))
    (when (and first-index (< first-index (1- (length line))))
      (string (aref line first-index) (seq-max (s-chop-left (1+ first-index) line))))))

(defun aoc/2025/3/max-joltage (line)
  (->> (number-sequence 9 1 -1)
       (-map (lambda (first-digit)
               (aoc/2025/3/max-by-first-digit line (number-to-string first-digit))
               ))
       (-first 'stringp)))

(defun aoc/2025/3a (filename)
  (->> filename (f-read) (s-trim) (s-lines)
       (-map 'aoc/2025/3/max-joltage)
       (-map 'string-to-number)
       (-sum)))

;; (aoc/2025/3a "input.txt")
