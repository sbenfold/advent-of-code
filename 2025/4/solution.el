(require 'dash)
(require 'f)
(require 's)

(defun aoc/2025/4/get-adjacents (data pos dim)
  (->> (-table '+ (-iota 3 (- dim) dim) (-iota 3 (- pos 1)))
       (-flatten)
       (--map (eq (aref data it) ?@))
       (-non-nil)
       (length)))

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
