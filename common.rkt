#lang racket

(require srfi/26) ; Cut macro for partial function application

;; Administration

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

;; Mapping

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

(define (slice-up lst n)
  "Slices up a list into sublists length n. Excess elements ignored."
  (define (aux lst acc)
    (if (< (length lst) n)
        (reverse acc)
        (aux (drop lst n) (cons (take lst n) acc))))
  (aux lst '()))

(define (pair-up lst)
  (slice-up lst 2))

;; Tuples are of form (x1 x2 . x3).

(define (tuple. proc . tuples)
  "Applies proc to tuples elementwise.
   Assumes same length tuples and at least one given element."
  (define (aux tuples)
    (if (pair? (car tuples))
        (cons (apply proc (map car tuples))
              (aux (map cdr tuples)))
        (apply proc tuples)))
  (aux tuples))

;; Some basic tuple operations

(define (+. . tuples)
  (apply tuple. + tuples))

(define (-. . tuples)
  (apply tuple. - tuples))

(define (*. . tuples)
  (apply tuple. * tuples))

(define (/. . tuples)
  (apply tuple. / tuples))

(define (mult2 tuple)
  "Multiply first two elements of a tuple"
  (if (pair? (cdr tuple))
      (* (car tuple) (cadr tuple))
      (* (car tuple) (cdr tuple))))

(define (tapply procs . tuples)
  "Applies procs to tuples componentwise. Assumes operations and vectors are all the same length"
  (if (pair? procs)
      (cons (apply (eval (car procs)) (map car tuples))
            (tapply (cdr procs) (map cdr tuples)))
      (apply (eval procs) (car tuples))))

(provide (all-defined-out))

