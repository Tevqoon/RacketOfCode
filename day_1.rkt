#lang racket

(require "common.rkt")
(define input (open-day 1))

(define (solver1 input)
  (define (aux lst acc)
    (cond ((empty? (cdr lst)) acc)
          ((< (car lst) (cadr lst)) (aux (cdr lst) (+ acc 1)))
          (#t (aux (cdr lst) acc))))
  (aux input 0))

(solver1 input)

(define (solver2 input)
  (solver1 (mapb + input (cdr input) (cddr input))))

(solver2 input)
