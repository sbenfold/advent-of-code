(require 'dash)
(require 'f)
(require 's)

(defun aoc/2025/1/read-file (filename)
  (->> filename (f-read) (s-trim) (s-lines)))

(defun aoc/2025/1a (input)
  (->> input
       (--map (->> it
                   (s-replace-all '(("L" . "-")
                                    ("R" . "+")))
                   (string-to-number)))
       (-reductions-from #'+ 50)
       (-map (lambda (x) (% x 100)))
       (-count 'zerop)))

(defun aoc/2025/1b (input)
  (let* ((deltas (--map (->> it
                             (s-replace-all '(("L" . "-")
                                              ("R" . "+")))
                             (string-to-number))
                        input))
         (points (->> deltas (-reductions-from #'+ 50)))
         (amt-to-add (--> (-min points) (/ it 100) abs 1+ (* it 100)))
         (pos-points (--map (+ it amt-to-add) points))
         (pairs (-zip-pair pos-points (cdr pos-points)))
         (intersections (-map (lambda (pair)
                                (-let* (((start . end) pair)
                                        (offset (if (< end start) -1 0)))
                                  (- (/ (+ end offset) 100)
                                     (/ (+ start offset) 100))
                                  )) pairs)))
    (->> intersections (-map 'abs) (-sum))))

(aoc/2025/1a (aoc/2025/1/read-file "input.txt"))
(aoc/2025/1b (aoc/2025/1/read-file "input.txt"))
