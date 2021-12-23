#lang racket

(require "common.rkt")

(define (processor input)
  (map (compose (λ(l) (list (string->number (cadr (string-split (car l) "=")))
                            (string->number (cadr l))))
                (curryr string-split "..")
                symbol->string) (cddr input)))

(define input (processor (open-day 17)))
(define test-input '((20 30) (-10 -5)))

(define (step point velocity)
  (values (+. point velocity)
          (+. velocity (list (- (sgn (car velocity))) -1))))

(define (stepchecker starting-velocity ranges)
  (define (aux acc point velocity)
    (cond [(andmap identity (map <= (map car ranges) point (map cadr ranges)))
           acc]
          [(or (<= (cadar ranges) (car point)) (<= (cadr point) -10000))
           #f]
          [else (let-values ([(new-point new-velocity) (step point velocity)])
                  (aux (max acc (cadr new-point)) new-point new-velocity))]))
  (aux (- (expt 10 10)) '(0 0) starting-velocity))

(define height-starting (filter identity
                                (map (curryr stepchecker input)
                                     (cartesian-product (range 1 200) (range -200 200)))))

(submit 1 (apply max height-starting))
(length height-starting)
