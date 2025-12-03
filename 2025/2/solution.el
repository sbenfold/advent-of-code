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
                                           (-map 'string-to-number)
                                           )))
       (-flatten)
       (-filter (lambda (x)
                  (let* ((s (number-to-string x))
                         (mid (/ (length s) 2)))
                    (and (evenp (length s))
                         (equal (s-left mid s)
                                (s-right mid s))))))
       (-sum)))

(aoc/2025/2a "input.txt")
