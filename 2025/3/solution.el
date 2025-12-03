(require 'dash)
(require 'f)
(require 's)

(defun aoc/2025/3/max-joltage (n line)
  (let* ((search-str (s-chop-right (1- n) line))
         (best-char (string (seq-max search-str)))
         (best-index (s-index-of best-char search-str))
         )
    (if (eq n 1)
        best-char
      (concat best-char
              (aoc/2025/3/max-joltage (1- n)
                                      (s-chop-left (1+ best-index) line))))))

(defun aoc/2025/3a (filename)
  (->> filename (f-read) (s-trim) (s-lines)
       (--map (aoc/2025/3/max-joltage 2 it))
       (-map 'string-to-number)
       (-sum)
       ))

;; (aoc/2025/3a "example.txt")
;; (aoc/2025/3a "input.txt")
