#lang racket

(require "common.rkt")
(require math/number-theory)

(define input (open-day 7))

(define (best crabs cost)
  (apply min (for/list ([pos (inclusive-range 0 (apply max crabs))])
               (foldl + 0 (map (cut cost <> pos) crabs)))))

(submit 1 (best input (λ(x pos) (abs (- x pos)))) #f)
(submit 2 (best input (λ(x pos) (binomial (add1 (abs (- x pos))) 2))) #f)
