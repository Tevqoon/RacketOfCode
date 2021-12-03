#lang racket

(require srfi/26) ; Cut macro for partial function application

;; Administration

(define (open-day n [lines? #f])
  "Opens the n-th day's input file as a list.
   If it doesn't exist, pull it with the aoc package."
  (let [(day-filename (format "day_~a.in" n))]
    (unless (file-exists? day-filename)
      (system (format "raco aoc -d ~a > ~a" n day-filename)))
    (if lines?
        (file->lines day-filename)
        (file->list day-filename))))

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

(define (map. proc . tuples)
  "Applies proc to tuples elementwise.
   Assumes same length tuples and at least one given element. Also works on lists."
  (define (aux tuples)
    (cond [(pair? (car tuples))
           (cons (apply proc (map car tuples))
              (aux (map cdr tuples)))]
          [(null? (car tuples)) null]
          [else (apply proc tuples)]))
  (aux tuples))

;; Some basic tuple operations

(define (+. . tuples)
  (apply map. + tuples))

(define (-. . tuples)
  (apply map. - tuples))

(define (*. . tuples)
  (apply map. * tuples))

(define (/. . tuples)
  (apply map. / tuples))

(define (mult2 tuple)
  "Multiply first two elements of a tuple"
  (if (pair? (cdr tuple))
      (* (car tuple) (cadr tuple))
      (* (car tuple) (cdr tuple))))

(define (tapply procs . tuples)
  "Applies procs to tuples componentwise. Assumes operations and tuples are all the same length"
  (if (pair? procs)
      (cons (apply (eval (car procs)) (map car tuples))
            (tapply (cdr procs) (map cdr tuples)))
      (apply (eval procs) (car tuples))))

;; Binary bullshit

(define (binstr->binlist binstr)
  (map (cut - <> 48)
       (map char->integer (string->list binstr))))

(define (binlist->binstr binlist)
  (list->string (map integer->char
                     (map (cut + <> 48) binlist))))

(define (binstr->integer binstr)
  (string->number (format "#b~a" binstr)))

(define (binlist->integer binlist)
  (binstr->integer (binlist->binstr binlist)))

(provide (all-defined-out))

