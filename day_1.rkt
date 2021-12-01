#lang racket

(require "common.rkt")

(define input (open-day 1))

(define (solver1 input)
  (count identity (mapcar < input (cdr input))))

(solver1 input)

(define (solver2 input)
  (solver1 (mapcar + input (cdr input) (cddr input))))

(solver2 input)
 
