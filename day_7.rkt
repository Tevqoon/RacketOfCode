#lang racket

(require "common.rkt")
(require math/number-theory)

(define input (open-day 7))

(define (cost-1 pos crabs)
  (foldl + 0 (map (λ(x) (abs (- x pos))) crabs)))

(define (cost-2 pos crabs)
  (foldl + 0 (map (λ(x) (binomial (add1 (abs (- x pos))) 2)) crabs)))

(define (best crabs cost)
  (apply min (for/list ([pos (inclusive-range 0 (apply max crabs))])
               (cost pos crabs))))

(submit 1 (best input cost-1) #f)
(submit 2 (best input cost-2) #f)
