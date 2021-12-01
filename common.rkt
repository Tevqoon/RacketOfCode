#lang racket

(define (open-day n)
  "Opens the n-th day's input file as a list.
   If it doesn't exist, pull it with the aoc package."
  (let [(day-filename (format "day_~a.in" n))]
    (unless (file-exists? day-filename)
      (system (format "raco aoc -d ~a > ~a" n day-filename)))
    (file->list day-filename)))

(define (unpack-lists lists)
  "Takes a list of lists and returns a list of their cars and a list of their cdrs.
   If any list is empty, return nil."
  (define (aux lists cars cdrs)
    (cond [(empty? lists) (cons (reverse cars) (reverse cdrs))]
          [(empty? (car lists)) '()]
          [#t (aux (cdr lists)
                   (cons (caar lists) cars)
                   (cons (cdar lists) cdrs))]))
  (aux lists '() '()))

(define (mapcar proc . lists)
  "Map proc over lists, returning when any one of them runs out."
  (define (aux lists acc)
    (let [(unpacked (unpack-lists lists))]
      (if (empty? unpacked)
          (reverse acc)
          (aux (cdr unpacked)
               (cons (apply proc (car unpacked))
                     acc)))))
  (aux lists '()))

(provide (all-defined-out))
