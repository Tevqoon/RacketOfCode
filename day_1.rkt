#lang racket

(require "common.rkt")
 
(define input (open-day 1))

(define (solver1 input)
  (count identity (mapcar < input (cdr input))))

(submit 1 (solver1 input))

(define (solver2 input)
  (solver1 (mapcar + input (cdr input) (cddr input))))

(submit 2 (solver2 input))
