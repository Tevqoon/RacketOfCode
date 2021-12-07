#lang racket

(require "common.rkt")

(define input (slice-up (open-day 5) 5))

(define (line->points line)
  (match line
    [(list x1 y1 _ x2 y2)
     (let* ([dx (- x2 x1)] [dy (- y2 y1)]
            [step (gcd dx dy)]
            [sx (/ dx step)] [sy (/ dy step)])
       (for/list ([i (inclusive-range 0 step)])                  
         (cons (+ x1 (* i sx)) (+ y1 (* i sy)))))]))

(define (solver lines)
  (let ([points (make-hash)])
    (for ([point (apply append (map line->points lines))])
      (hash-update! points point add1 0))
    (count (cut <= 2 <>) (hash-values points))))

(define (horver? line)
  (match line [(list x1 y1 _ x2 y2) (or (= x1 x2) (= y1 y2))]))

(submit 1 (solver (filter horver? input)) #f)
(submit 2 (solver input) #f)
