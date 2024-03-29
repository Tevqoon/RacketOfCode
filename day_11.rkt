#lang racket

(require "common.rkt")

(define input (map binstr->binlist (open-day 11 #t)))
(define x0 (length input))
(define y0 (length (car input)))
(define directions (cartesian-product '(-1 0 1) '(-1 0 1)))

(define (step input)
  (define (aux board to-flash)
    (match to-flash
      [(cons x xs) #:when (not (lrr board x)) (aux board xs)]
      [(cons x xs) #:when (< 8 (lrr board x))
                   (aux (lrrp board x #f)
                        (append to-flash (neighbors x directions x0 y0)))]
      [(cons x xs) (aux (lrrp board x (add1 (lrr board x))) xs)]
      [empty (map (curry map (λ(x) (if x x 0))) board)]))
  (aux input (cartesian-product (range x0) (range y0))))

(define (solver1 input steps)
  (define (aux acc board n)
    (if (= n steps)
        acc
        (let ([new-board (step board)])
          (aux (+ acc (count zero? (flatten new-board)))
               new-board
               (add1 n)))))
  (aux 0 input 0))

(define (solver2 input)
  (define (aux board n)
    (if (empty? (filter-not zero? (flatten board)))
        n
        (aux (step board) (add1 n))))
  (aux input 0))

(submit 1 (solver1 input 100) #f)
(submit 2 (solver2 input) #f)
