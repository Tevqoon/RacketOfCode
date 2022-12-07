#lang racket

(require "common.rkt")
(require repeated-application)

(define input (map (compose (curryr call-with-input-string read)
                            (curryr string-replace "," " "))
                   (file->lines "day_18.in")))

(define (split sail)
  (match sail
    [a #:when (and (number? a) (< 9 a))
       (list (floor (/ a 2)) (ceiling (/ a 2)))]
    [a #:when (number? a) a]
    [(list a b) (let ([left-split (split a)])
                  (if (equal? a (split a))
                      (list a (split b))
                      (list left-split b)))]))
    
(define (add-right num sail)
  (if (number? sail) (+ num sail) (list (add-right num (car sail)) (cadr sail)))) 
  
(define (add-left num sail)
  (if (number? sail) (+ num sail) (list (car sail) (add-left num (cadr sail)))))

(define (explode sail [k (λ(x f) x)] [depth 0])
  (match sail
    [a #:when (number? a) (k a '(#f #f))]
    [(list a b) #:when (= depth 4) (k 0 (list a b))]
    [(list a b) (let ([left-explode (explode a
                                             (λ(x f) 
                                               (cond [(cadr f) (list (list x (add-right (cadr f) b))
                                                                     (list (car f) #f))]
                                                     [#t (list (list x b) f)]))
                                             (+ 1 depth))])
                  (if (equal? (car left-explode) (list a b))
                      (let ([right-explode (explode b
                                                    (λ(y f) 
                                                      (cond [(car f) (list (list (add-left (car f) a) y)
                                                                           (list #f (cadr f)))]
                                                            [#t (list (list a y) f)]))
                                                    (+ 1 depth))])
                        (apply k right-explode))
                      (apply k left-explode)))]))

(define (reduce sail)
  (apply↑* (λ(x) (split (apply↑* explode x))) sail))

(define (add sail1 sail2)
  (reduce (list sail1 sail2)))

(define (magnitude sail)
  (match sail
    [a #:when (number? a) a]
    [(list a b) (+ (* 3 (magnitude a)) (* 2 (magnitude b)))]))

(define (folder acc sails)
  (if (null? sails)
      (magnitude acc)
      (folder (add acc (car sails)) (cdr sails))))

(displayln (folder (car input) (cdr input)))
(displayln (apply max (map (compose magnitude (curry apply add))
                           (cartesian-product input input))))

