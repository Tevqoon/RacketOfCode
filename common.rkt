#lang racket

(define (open-day n)
  "Opens the n-th day's input file as a list.
   If it doesn't exist, pull it with the aoc package."
  (let [(day-filename (format "day_~a.in" n))]
    (unless (file-exists? day-filename)
      (system (format "raco aoc -d ~a > ~a" n day-filename)))
    (file->list day-filename)))

(define (submit n val [submit? #f])
  (if submit?
      (system (format "raco aoc -a ~a ~a" n val))
      val))

(define (mapcar f . xss)
  "A mufch better mapcar stolen from Muf."
  (define (aux acc . xss)
    (if (ormap empty? xss)
        (reverse acc)
        (apply aux (cons (apply f (map car xss)) acc) (map cdr xss))))
  (apply aux '() xss))

(define (reduce f xs)
  "Literally just reduce smfh."
  (and (not (empty? xs)) (foldl f (first xs) (rest xs))))

(define (pair-up lst n)
  "Slices up a list into sublists length n. Excess elements ignored."
  (define (aux lst acc)
    (if (< (length lst) n)
        (reverse acc)
        (aux (drop lst n) (cons (take lst n) acc))))
  (aux lst '()))

;; Tuples are of form (x1 x2 . x3) etc. List with last el non-nil.

(define (+. . tuples)
  "Tuple addition. Assumes at least one given element."
  (if (pair? (car tuples))
      (cons (apply +  (map car tuples))
            (apply +. (map cdr tuples)))
      (apply + tuples)))

(define (mult2 tuple)
  "Multiply first two elements of a tuple"
  (if (pair? (cdr tuple))
      (* (car tuple) (cadr tuple))
      (* (car tuple) (cdr tuple))))

(provide (all-defined-out))
