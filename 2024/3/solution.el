;;; solution.el --- Advent of Code 2024 Day 3 solution  -*- lexical-binding: t -*-

(require 'generator)

(defun aoc/3/read-input ()
  ;; (->> (f-read-text "example-input.txt")
  (->> (f-read-text "input.txt")
     (s-trim)))
;; (aoc/3/read-input)

(defun aoc/3/parse ()
  (save-current-buffer
    (set-buffer "example-input.txt")
    (goto-char (point-min))
    (let ((matches '()))
      (while (re-search-forward "mul(\\([0-9]+\\),\\([0-9]+\\))" nil t)
        ;; (message "Found")
        ;; (message (match-string 1))
        ;; (iter-yield (cons (match-string 1) (match-string 2)))
        ;; (iter-yield 3)
        ;; (push (cons (match-string 1) (match-string 2)) matches)
        (setq matches (match-string 1))
        )
      matches
      )))

(iter-defun aoc/3/parse2 ()
  (let* ((input (aoc/3/read-input)))
    (while (re-search-forward "mul(\\([0-9]+\\),\\([0-9]+\\))" nil t)
      (iter-yield (cons (match-string 1) (match-string 2)))
      )
    ))

(arrayp (car (aoc/3/parse)))
(+ (car (aoc/3/parse)) 2)

(cl-loop for x
         iter-by (aoc/3/parse2)
         collect x)

(let ((input "xmul(2,4)%&mul[3,7]"))
  (string-match "mul(\\([0-9]+\\),\\([0-9]+\\))" input)
  (message "%s %s" (match-string 1 input) (match-string 2 input)))

(iter-defun aoc/3/parse3 ()
  (let* ((input (aoc/3/read-input))
         (pos 0))
    (while (string-match "mul(\\([0-9]+\\),\\([0-9]+\\))" input pos)
      (iter-yield (cons (string-to-number (match-string 1 input))
                        (string-to-number (match-string 2 input))))
      (setq pos (match-end 0)))))

(->>
 (cl-loop for x
          iter-by (aoc/3/parse3)
          collect x)
 ;; (apply #'-zip-with #'*)
 (--map (* (car it) (cdr it)))
 (-sum)
 )

;; (get-or-create-buffer "*helm find files*")
;; (create-file-buffer "*helm find files*")
