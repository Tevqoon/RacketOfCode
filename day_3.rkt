#lang racket

(require "common.rkt")
(require srfi/26) ; cut macro

(define input (map binstr->binlist (open-day 3 #t)))

(define (decider comparator input)
  (binlist->integer
   (map (λ (x)
          (if (comparator x (/ (length input) 2)) 1 0))
        (apply +. input))))

(submit 1 (* (decider <= input) (decider >= input)) #f)

(define (solver2 comparator input)
  (define (aux input n)
    (if (null? (cdr input))
        (car input)
        (let* ([freq (apply + (map (cut list-ref <> n) input))]
               [bit  (if (comparator freq (/ (length input) 2)) 1 0)])
          (aux (filter (λ (x) (equal? (list-ref x n) bit)) input) (+ n 1)))))
  (binlist->integer (aux input 0)))

(submit 2 (* (solver2 < input) (solver2 >= input)) #f)
