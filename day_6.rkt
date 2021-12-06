#lang racket

(require "common.rkt")

(define input (open-day 6))

(define (stepper fish days)
  (let ([fishes (for/list ([i (range 9)]) (count (cut = i <>) fish))])
    (define (aux hd tl n)
      (if (zero? n)
          (+ (apply + hd) (apply + tl))
          (aux (append (cdr hd) (take tl 1))
               (list (+ (car hd) (cadr tl)) (caddr tl) (car hd))
               (sub1 n))))
    (let-values ([(hd tl) (split-at fishes 6)])
      (aux hd tl days))))

(submit 1 (stepper input 80) #f)
(submit 2 (stepper input 256) #f)
