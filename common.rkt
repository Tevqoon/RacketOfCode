#lang racket

(define (open-day n)
  (let* [(day-str (~v n))
         (day-filename (string-append "day_" day-str ".in"))]
    (unless (file-exists? day-filename)
      (system (string-append "raco aoc -d " day-str " > " day-filename)))
    (file->list day-filename)))

(define (unpack-lists lsts)
  "Takes a list of lists and returns a list of their cars and a list of their cdrs.
   If any list is empty, return nil."
  (define (aux lsts cars cdrs)
    (cond ((empty? lsts) (cons (reverse cars) (reverse cdrs)))
          ((empty? (car lsts)) '())
          (#t (aux (cdr lsts) (cons (caar lsts) cars) (cons (cdar lsts) cdrs)))))
  (aux lsts '() '()))

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
