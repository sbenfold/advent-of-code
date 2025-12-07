(require 'dash)
(require 'f)
(require 's)

(defun aoc/2025/4/read-file (filename)
  (->> filename (f-read) (s-trim) (s-lines) (s-join "")))

;; TODO No need to unpad
(defun aoc/2025/4/unpad (padded dim)
  ;; (->> padded
  ;;      (s-chop-left (+ dim 2))
  ;;      (s-chop-right (+ dim 2))
  ;;      ;; TODO Remove "." from start and end of each line
  ;;      )
  (s-replace "+" "" padded)
  )
(aoc/2025/4/unpad "++++++..@++@@@++@@@++++++" 3)

(defun aoc/2025/4/get-adjacents (data pos dim)
  (let* ((offsets (-flatten (-table 'cons (-iota 3 -1) (-iota 3 -1))))
         (pos-offsets (--map (+ (* (car it) dim) (cdr it) pos) offsets))
         )
    ;; TODO This is the expanded dim
    (->> pos-offsets
         (--map (eq (aref data it) ?.))
         (-non-nil)
         (length)
         )
    ))
(aoc/2025/4/get-adjacents "........@..@@@..@@@......" 12 5)

(defun aoc/2025/4/read-file-expanded (filename)
  (let* ((lines (->> filename (f-read) (s-trim) (s-lines)))
         (dim (length (car lines)))
         (pad-line (s-repeat (+ dim 2) "."))
         (data (->> lines
                    (--map (s-wrap it "." "."))
                    (s-join "")
                    ))
         (data-padded (concat pad-line data pad-line))
         )
    (->>
     (s-matched-positions-all "@" data-padded)
     ;; (-unzip-lists)
     (-map 'car)
     )
    ;; data-padded
    ))
(aoc/2025/4/read-file-expanded "example2.txt")

(length (aoc/2025/4/read-file "input.txt"))

;; TODO Filter by < 4
