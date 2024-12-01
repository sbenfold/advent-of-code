(require 'dash)
(require 'f)
(require 's)

;; 1a
(->> (f-read-text "input.txt")
   (s-trim)
   (s-lines)
   (-map #'s-split-words)
   (-map (-partial #'-map #'string-to-number))
   (-unzip)
   (--map (-sort #'< it))
   (apply #'-zip-with #'-)
   (-map #'abs)
   (-sum)
   )

;; 1b
(-let* (((a b) (->> (f-read-text "input.txt")
                  (s-trim)
                  (s-lines)
                  (-map #'s-split-words)
                  (-map (-partial #'-map #'string-to-number))
                  (-unzip)))
        (freqs (-frequencies b)))
  (->> a
     (--map (* it (alist-get it freqs 0)))
     (-sum)))
