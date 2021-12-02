#lang racket

(require "common.rkt")
(define input (pair-up (open-day 2) 2))

(define (decoder1 pair acc)
  (+. acc (match pair
        [(list 'forward val) `(,val . 0)]
        [(list 'down val)    `(0 . ,val)]
        [(list 'up val)      `(0 . ,(- val))])))

(define (decoder2 pair acc)
  (+. acc (match pair
        [(list 'forward val) `(,val ,(* val (cddr acc)) . 0)]
        [(list 'down val)    `(0 0 . ,val)]
        [(list 'up val)      `(0 0 . ,(- val))])))

(define (solver decoder start)
  (mult2 (foldl decoder start input)))

(submit 1 (solver decoder1 '(0 . 0)) #f)
(submit 2 (solver decoder2 '(0 0 . 0)) #f)
