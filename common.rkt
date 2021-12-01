#lang racket

(define (open-day n)
  (file->list (string-append "day_" (~v n) ".in")))

(define (unpack-lists rest-id)
  "Returns a list of cars and a list of cdrs. If any list is empty, return nil."
  (define (aux lsts cars cdrs)
    (cond ((empty? lsts) (cons (reverse cars) (reverse cdrs)))
          ((empty? (car lsts)) '())
          (#t (aux (cdr lsts) (cons (caar lsts) cars) (cons (cdar lsts) cdrs)))))
  (aux rest-id '() '()))

(define (mapb proc . lsts)
  "Map proc over lsts, returning when any one of them runs out."
  (define (aux lsts acc)
    (let [(unpacked (unpack-lists lsts))]
      (if (empty? unpacked)
          (reverse acc)
          (aux (cdr unpacked) (cons (apply proc (car unpacked))
                                    acc)))))
  (aux lsts '()))

(provide (all-defined-out))
