(require 'dash)
(require 'f)
(require 's)

(defun aoc/2025/4/get-adjacents (data pos dim)
  (->> (-table-flat '+ (-iota 3 (- dim) dim) (-iota 3 (- pos 1)))
       (--map (eq (aref data it) ?@))
       (-count 'identity)))

(defun aoc/2025/4/a (filename)
  (let* ((lines (->> filename (f-read) (s-trim) (s-lines)))
         (dim (length (car lines)))
         (pad-line (s-repeat (+ dim 2) "."))
         (data (->> lines
                    (--map (s-wrap it "." "."))
                    (s-join "")))
         (data-padded (concat pad-line data pad-line)))
    (->>
     (s-matched-positions-all "@" data-padded)
     (-map 'car)
     (--map (aoc/2025/4/get-adjacents data-padded it (+ dim 2)))
     (--filter (< it 5))
     (length))))

(aoc/2025/4/a "example2.txt")
(aoc/2025/4/a "example.txt")
(aoc/2025/4/a "input.txt")

;; TODO Remove matches from board and recurse
(defun aoc/2025/4/b-helper (data-padded dim)
  (let* ((removable (->>
                     (s-matched-positions-all "@" data-padded)
                     (-map 'car)
                     ;; TODO For each position matching filter, set it to ".". Recurse if we changed anything.
                     (--annotate (aoc/2025/4/get-adjacents data-padded it (+ dim 2)))
                     (--filter (< (car it) 5))
                     (-map 'cdr)
                     ;; (length)
                     )
                    ))
    (if (null removable)
        data-padded
      (--each removable (aset data-padded it ?.))
      (aoc/2025/4/b-helper data-padded dim)
      )
    ))

(defun aoc/2025/4/b (filename)
  (let* ((lines (->> filename (f-read) (s-trim) (s-lines)))
         (dim (length (car lines)))
         (pad-line (s-repeat (+ dim 2) "."))
         (data (->> lines
                    (--map (s-wrap it "." "."))
                    (s-join "")))
         (data-padded (concat pad-line data pad-line))
         (final-state (aoc/2025/4/b-helper data-padded dim))
         (initial-num (length (s-matched-positions-all "@" data)))
         (final-remaining (length (s-matched-positions-all "@" final-state)))
         )
    (- initial-num final-remaining)
    )
  )

(aoc/2025/4/b "example2.txt")
(aoc/2025/4/b "example.txt")
(aoc/2025/4/b "input.txt")
