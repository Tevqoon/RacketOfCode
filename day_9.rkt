#lang racket

(require "common.rkt")

(define input (map binstr->binlist (open-day 9 #t)))
(define directions '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))

(define (lrr l x)
  (list-ref (list-ref l (car x)) (cdr x)))

(define (is? prd el rst)
  (andmap (λ(x) (prd el x)) rst))

(define (low-points input)
  (let ([padded (pad input +inf.0)])
    (apply
     append
     (for/list ([x (range 1 (add1 (length input)))])
       (for/list ([y (range 1 (add1 (length (car input))))]
                  #:when (is? < (lrr padded (cons x y))
                              (map (λ(d) (lrr padded (+. (cons x y) d))) directions)))
         (cons (sub1 x) (sub1 y)))))))

(define (get-basin-size input low-point)
  (define (neighbors point)
    (filter (λ(x) (and (<. x (cons (length input) (length (car input))))
                       (>. x '(0 . 0))))
            (map (cut +. point <>) directions)))
  (define (aux size seen to-look)
    (match to-look
      [(cons x xs)
       (if (member x seen)
           (aux size seen xs)
           (aux (add1 size)
                (cons x seen)
                (append xs (filter (λ(y) (and (not (= (lrr input y) 9))
                                              (<. (lrr input x) (lrr input y))))
                                   (neighbors x)))))]
      [empty size]))
  (aux 0 '() (list low-point)))

(define (solver1 input)
  (apply + (map (λ(c) (add1 (lrr input c))) (low-points input))))

(define (solver2 input)
  (apply * (take (sort (map (cut get-basin-size input <>) (low-points input)) >=) 3)))

(submit 1 (solver1 input) #f)
(submit 2 (solver2 input) #f)
