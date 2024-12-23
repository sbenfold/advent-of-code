;;; solution.el --- Advent of Code 2024 Day 9 solution  -*- lexical-binding: t -*-

;; (defconst aoc/9/input-file "input.txt")
(defconst aoc/9/input-file "example-input.txt")

(defun aoc/9/expand-disk-pair (index pair)
  (concat
   (s-repeat (car pair) (number-to-string index))
   (s-repeat (cadr pair) ".")))

(defun aoc/9/to-block-map (disk-map)
  (->> (concat disk-map "0")
     (s-split "")
     (-remove #'s-blank?)
     (-map #'string-to-number)
     (-partition 2)
     (-map-indexed #'aoc/9/expand-disk-pair)))

(defun aoc/9/compress-map (block-map)
  (let ((index 0)
        (end-index (1- (length block-map))))
    (while (< index end-index)
      (when (eq (aref block-map index) ?.)
        (aset block-map index (aref block-map end-index))
        (setq end-index (1- end-index))
        (while (eq (aref block-map end-index) ?.)
          (setq end-index (1- end-index))))
      (setq index (1+ index)))
    (s-left (1+ end-index) block-map)))

(defun aoc/9/solve ()
  (let* ((compressed (->> (f-read-text aoc/9/input-file)
                        (s-trim)
                        (aoc/9/to-block-map)
                        (s-join "")
                        (aoc/9/compress-map))))
    (->> compressed
       ;; (s-split "")
       ;; (-remove #'s-blank?)
       ;; (-map #'string-to-number)
       ;; (-zip-with #'* (-iota (length compressed)))
       ;; (-sum)
       )))

;; 9a
;; TODO 92092395920 is too low
(aoc/9/solve)
