(require 'dash)
(require 'f)
(require 's)

;; 1a
(defun aoc/1/read-input ()
  (->> (f-read-text "input.txt")
     (s-trim)
     (s-lines)
     (-map #'s-split-words)
     (-map (-partial #'-map #'string-to-number))
     (-unzip)))

(->> (aoc/1/read-input)
   (--map (-sort #'< it))
   (apply #'-zip-with #'-)
   (-map #'abs)
   (-sum))

;; 1b
(-let* (((a b) (aoc/1/read-input))
        (freqs (-frequencies b)))
  (->> a
     (--map (* it (alist-get it freqs 0)))
     (-sum)))
