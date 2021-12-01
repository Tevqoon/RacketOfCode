#lang racket

(define (open-day n)
  (let* [(day-str (~v n))
         (day-filename (string-append "day_" day-str ".in"))]
    (if (file-exists? day-filename)
        (file->list day-filename)
        (begin (system (string-append "raco aoc -d " day-str " > " day-filename))
               (open-day n)))))

(define (unpack-lists rest-id)
  "Returns a cons of cars and a list of cdrs. If any list is empty, return nil."
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
