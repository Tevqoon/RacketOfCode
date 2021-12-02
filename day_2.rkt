#lang racket

(require "common.rkt")
(define input (pair-up (open-day 2)))

(define (decoder pair acc)
  (+. acc (match pair
            [(list 'forward val) `(0 ,val  . ,(* val (car acc)))]
            [(list 'down val)    `(,val 0 . 0)]
            [(list 'up val)      `(,(- val) 0 . 0)])))

(define (solver input)
  (foldl decoder '(0 0 . 0) input))

(submit 1 (mult2 (solver input)) #f)
(submit 2 (mult2 (cdr (solver input))) #f)
