;;; solution.el --- Advent of Code 2024 Day 5 solution  -*- lexical-binding: t -*-

;; Default
(setq max-lisp-eval-depth 1600)
;; 5x default
;; (setq max-lisp-eval-depth 8000)

(require 'generator)
(require 'ht)

;; (defconst aoc/5/input-file "input.txt")
(defconst aoc/5/input-file "example-input.txt")

(defun aoc/5/validate-update (pages deps-ht)
  (-all? #'null
         (--map
          (-intersection
           (ht-get deps-ht (nth it pages))
           (-take it pages)
           )
          (-iota (length pages) 1)
          )))

;; 5a
(-let* ((lines (s-lines (f-read-text aoc/5/input-file)))
        ((deps-raw . updates-raw) (-split-on "" lines))
        (deps (->> deps-raw
                 (-sort #'s-less?)
                 (--map (s-split "|" it))
                 (-map (-partial #'-map #'string-to-number))
                 (-partition-by #'car)
                 ))
        (updates (->> updates-raw
                    (car)
                    (--map (s-split "," it))
                    (-map (-partial #'-map #'string-to-number))
                    ))
        (deps-ht (ht-create))
        )
  (->> deps
     (--map
      (let ((unzipped (-unzip-lists it)))
        (ht-set! deps-ht (caar unzipped) (cadr unzipped))
        )
      ))
  (->> updates
     (--filter (aoc/5/validate-update it deps-ht))
     (--map (nth (/ (length it) 2) it))
     (-sum)
     )
  )

(defun aoc/5b/fix-update (pages deps-ht)
  ;; TODO: Take the first error and recurse with the missing dependency moved in front
  ;;   So if '(75 97 47 61 53) fails on 97 because 97 should've come first
  ;;   Recurse with '(97 75 47 61 53)
  ;; TODO We want the index of the first failure (add 1 to index)
  (-if-let
      (error-index
       (--find-index
        (-intersection
         (ht-get deps-ht (nth it pages))
         (-take it pages)
         )
        (-iota (length pages) 1)
        ))
      (let ((intersection (-intersection
                           (ht-get deps-ht (nth (1+ error-index) pages))
                           (-take (1+ error-index) pages)
                           )))
        ;; TODO Instead of moving dependent in front of us, move us to before dependency
        (let* ((old-index (-elem-index (car intersection) pages))
               (error-val (nth (1+ error-index) pages))
               (new-pages (-insert-at old-index error-val (-remove-at (1+ error-index) pages)))
               ;; (-insert-at (+ error-index 2) (nth old-index pages) pages)
               )
          ;; new-pages
          ;; (message "Recurse %s -> %s" pages new-pages)
          ;; (message "Recurse error index %s -> %s" new-pages (--find-index
          ;;                                                   (-intersection
          ;;                                                    (ht-get deps-ht (nth it new-pages))
          ;;                                                    (-take it new-pages)
          ;;                                                    )
          ;;                                                   (-iota (length new-pages) 1)
          ;;                                                   ))
          (aoc/5b/fix-update new-pages deps-ht)
          )

        )
    pages
    )
  )

;; 5b
(-let* ((lines (s-lines (f-read-text aoc/5/input-file)))
        ((deps-raw . updates-raw) (-split-on "" lines))
        (deps (->> deps-raw
                 (-sort #'s-less?)
                 (--map (s-split "|" it))
                 (-map (-partial #'-map #'string-to-number))
                 (-partition-by #'car)
                 ))
        (updates (->> updates-raw
                    (car)
                    (--map (s-split "," it))
                    (-map (-partial #'-map #'string-to-number))
                    ))
        (deps-ht (ht-create))
        )
  (->> deps
     (--map
      (let ((unzipped (-unzip-lists it)))
        ;; (message "Add: %s -> %s" (caar unzipped) (cadr unzipped))
        (ht-set! deps-ht (caar unzipped) (cadr unzipped))
        )
      ))

  ;; (let ((pages (list 75 97 47 61 53)))
  ;;   (aoc/5b/fix-update pages deps-ht)
  ;;   ;; (ht-get deps-ht (nth 1 pages))
  ;;   ;; (-take 1 pages)
  ;;   )
  (->> updates
     (--remove (aoc/5/validate-update it deps-ht))
     (--map (aoc/5b/fix-update it deps-ht))
     (--map (nth (/ (length it) 2) it))
     (-sum)
     )
  )
