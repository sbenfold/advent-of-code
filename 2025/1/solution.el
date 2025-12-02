(require 'dash)
(require 'f)
(require 's)

(defun aoc/2025/1/read-file (filename)
  (->> filename (f-read) (s-trim) (s-lines)))

(defun aoc/2025/1 (input)
  (let* ((deltas (--map (->> it
                             (s-replace-all '(("L" . "-")
                                              ("R" . "+")))
                             (string-to-number))
                        input))
         (result (->> deltas
                      (-reductions-from #'+ 50)
                      (-map (lambda (x) (% x 100)))
                      (-count 'zerop)
                      )))
    result))

(aoc/2025/1 (aoc/2025/1/read-file "input.txt"))

(defun aoc/2025/1b (input)
  (let* ((deltas (--map (->> it
                             (s-replace-all '(("L" . "-")
                                              ("R" . "+")))
                             (string-to-number))
                        input))
         (red (->> deltas
                   (-reductions-from #'+ 50)))
         (min (-min red))
         (amt-to-add (--> min (/ it 100) abs 1+ (* it 100)))
         (positive (--map (+ it amt-to-add) red))
         (pos-zipped (-zip-pair positive (cdr positive)))
         (cycles (-map (lambda (pair)
                         (let* ((start (car pair))
                                (end (cdr pair))
                                (diff (- end start))
                                (sign (if (< diff 0) -1 0)))
                           (- (/ (+ end sign) 100) (/ (+ start sign) 100))
                           )) pos-zipped))
         (result (->> cycles (-map 'abs) (-sum))))
    result))

(aoc/2025/1b (aoc/2025/1/read-file "input.txt"))
