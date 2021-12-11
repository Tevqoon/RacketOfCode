#lang racket

(require "common.rkt")

(define input (map binstr->binlist (open-day 9 #t)))
(define x0 (length input))
(define y0 (length (car input)))
(define directions '((1 0) (-1 0) (0 1) (0 -1)))

(define (low-points input)
  (let ([padded (pad input +inf.0)])
    (for/list ([x (cartesian-product (inclusive-range 1 x0) (inclusive-range 1 y0))]
               #:when (andmap (λ(d) (< (lrr padded x) (lrr padded (map + x d))))
                              directions))
      (map - x '(1 1)))))

(define (get-basin-size input low-point)
  (define (aux size seen to-look)
    (match to-look
      [(cons x xs) #:when (set-member? seen x) (aux size seen xs)]
      [(cons x xs)
       (aux (add1 size)
            (set-add seen x)
            (append xs (filter (λ(y) (< (lrr input x) (lrr input y) 9))
                               (neighbors x directions x0 y0))))]
      [empty size]))
  (aux 0 (set) (list low-point)))

(define (solver1 input)
  (apply + (map (compose add1 (curry lrr input)) (low-points input))))

(define (solver2 input)
  (apply * (take (sort (map (curry get-basin-size input) (low-points input)) >) 3)))

(submit 1 (solver1 input) #f)
(submit 2 (solver2 input) #f)