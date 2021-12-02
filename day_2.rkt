#lang racket

(require "common.rkt")

(define input (open-day 2))

(define (solver1 input)
  (define (aux lst x y)
    (match lst
      ['() (* x y)]
      [(list _) (* x y)]
      [(cons 'forward (cons val xss)) (aux xss (+ x val) y)]
      [(cons 'down (cons val xss)) (aux xss x (+ y val))]
      [(cons 'up (cons val xss)) (aux xss x (- y val))]))
  (aux input 0 0))

(submit 1 (solver1 input) #f)

(define (solver2 input)
  (define (aux lst x y aim)
    (match lst
      ['() (* x y)]
      [(cons 'down (cons val xss)) (aux xss x y (+ aim val))]
      [(cons 'up (cons val xss)) (aux xss x y (- aim val))]
      [(cons 'forward (cons val xss)) (aux xss (+ x val) (+ y (* val aim)) aim)]))
  (aux input 0 0 0))

(submit 2 (solver2 input) #f)
