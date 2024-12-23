;;; solution.el --- Advent of Code 2024 Day 9 solution  -*- lexical-binding: t -*-

;; (defconst aoc/9/input-file "input.txt")
(defconst aoc/9/input-file "example-input.txt")

(defun aoc/9/expand-disk-pair (index pair)
  (append
   (-repeat (car pair) index)
   (-repeat (cadr pair) nil)))

(defun aoc/9/to-block-map (disk-map)
  (->> (concat disk-map "0")
     (s-split "")
     (-remove #'s-blank?)
     (-map #'string-to-number)
     (-partition 2)
     (-map-indexed #'aoc/9/expand-disk-pair)
     (-flatten-n 1)
     (append)
     ))

(defun aoc/9/compress-map (block-map)
  (let ((index 0)
        (end-index (1- (length block-map))))
    (while (< index end-index)
      (when (null (aref block-map index))
        (aset block-map index (aref block-map end-index))
        (setq end-index (1- end-index))
        (while (null (aref block-map end-index))
          (setq end-index (1- end-index))))
      (setq index (1+ index)))
    (s-left (1+ end-index) block-map)))

(defun aoc/9/solve ()
  (let* ((block-map-list (->> (f-read-text aoc/9/input-file)
                            (s-trim)
                            (aoc/9/to-block-map)
                            ))
         (block-map-vec (cl-coerce block-map-list 'vector))
         (compressed (aoc/9/compress-map block-map-vec))
         (compressed-list (cl-coerce block-map-vec 'list))
         )
    (->> compressed-list
       (-zip-with #'* (-iota (length compressed)))
       (-sum)
       )
    ))

;; 9a
(aoc/9/solve)
