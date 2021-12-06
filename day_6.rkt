#lang racket

(require "common.rkt")

(define input (open-day 6))

(define (stepper fish days)
  (let ([fishes (make-vector 9)])
    (for ([fish fish])
      (vector-set! fishes fish (add1 (vector-ref fishes fish))))
    (define (aux hd tl n)
      (if (zero? n)
          (apply + (append hd tl))
          (aux (append (cdr hd) (take tl 1))
               (list (+ (car hd) (cadr tl)) (caddr tl) (car hd))
               (sub1 n))))
    (let-values ([(hd tl) (split-at (vector->list fishes) 6)])
      (aux hd tl days))))

(submit 1 (stepper input 80) #f)
(submit 2 (stepper input 256) #f)
