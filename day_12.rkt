#lang racket

(require "common.rkt")
(require memoize)

(define (collect pairs)
  (let ([rethash (make-hash)])
    (for ([pair pairs])
      (hash-update! rethash (car  pair) (curry cons (cadr pair)) '())
      (hash-update! rethash (cadr pair) (curry cons (car  pair)) '()))
    rethash))

(define (downcase? str)
  (equal? (string-downcase str) str))

(define input (collect (map (cut string-split <> "-") (open-day 12 #t))))

(define (explore1 caves)
  (define/memo (aux acc ongoing)
    "Accumulator has completed paths."
    (match ongoing
      [(cons x xs) #:when (equal? (car x) "end")
                   (aux (cons (reverse x) acc) xs)]
      [(cons x xs) (aux acc
                        (append (map (cut cons <> x)
                                     (set-subtract (hash-ref caves (car x))
                                                   (filter downcase? (cdr x))))
                                xs))]
      [empty acc]))
  (aux '() '(("start"))))

(define (explore2 caves)
  (define/memo (aux acc ongoing)
    "Accumulator has completed paths."
    (match ongoing
      [(cons x xs) #:when (equal? (car x) "end")
                   (aux (cons (reverse x) acc) xs)]
      [(cons x xs) (aux acc
                        (append (map (cut cons <> x)
                                     (if (check-duplicates (filter downcase? x))
                                         ;if there's already two small caves, filter them aggressively
                                         (set-subtract (hash-ref caves (car x))
                                                       (filter downcase? (cdr x)))
                                         ;othwerise just take all small caves as options except "start"
                                         (set-subtract (hash-ref caves (car x))
                                                       (list "start"))))
                                xs))]
      [empty acc]))
  (aux '() '(("start"))))

(submit 1 (length (explore1 input)) #f)
(submit 2 (length (explore2 input)) #f)
