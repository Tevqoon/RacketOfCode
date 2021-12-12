#lang racket

(require "common.rkt")

(define (collect pairs)
  (let ([rethash (make-hash)])
    (for ([pair pairs])
      (hash-update! rethash (car  pair) (curry cons (cadr pair)) '())
      (hash-update! rethash (cadr pair) (curry cons (car  pair)) '()))
    rethash))

(define (downcase? str)
  (equal? (string-downcase str) str))

(define input (collect (map (cut string-split <> "-") (open-day 12 #t))))

(define (explore caves disallow-duplicate?)
  (define (aux acc ongoing)
    (match ongoing
      [(cons x xs) #:when (equal? (car x) "end")
                   (aux (add1 acc) xs)]
      [(cons x xs)
       (aux acc
            (append (map (cut cons <> x)
                         (if (or disallow-duplicate? ; If duplicates aren't allowed, just don't care
                                 (check-duplicates (filter downcase? x)))
                             ;if there's already two small caves, filter them aggressively
                             (set-subtract (hash-ref caves (car x))
                                           (filter downcase? x))
                             ;othwerise just take all small caves as options except "start"
                             (set-subtract (hash-ref caves (car x))
                                           (list "start"))))
                    xs))]
      [empty acc]))
  (aux 0 '(("start"))))

(submit 1 (explore input #t) #f)
(submit 2 (explore input #f) #f)
