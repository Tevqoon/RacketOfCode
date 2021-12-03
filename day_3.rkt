#lang racket

(require "common.rkt")
(require srfi/26)

(define input (map binstr->binlist (open-day 3 #t)))

(define test
  (map binstr->binlist
       '("00100"
         "11110"
         "10110"
         "10111"
         "10101"
         "01111"
         "00111"
         "11100"
         "10000"
         "11001"
         "00010"
         "01010")))

(define (solver1 input)
  (let* ([l (length input)]
         [freqs (apply +. input)]
         [gamma   (map (λ (x) (if (>= x (/ l 2)) 1 0)) freqs)]
         [epsilon (map (λ (x) (if (<= x (/ l 2)) 1 0)) freqs)])
    (cons gamma epsilon)))

(define sol1 (solver1 input))
(submit 1 (* (binlist->integer (car sol1)) (binlist->integer (cdr sol1))) #f)

(define (finder2 op input)
  (define (aux input index)
    (if (= 1 (length input))
        (car input)
        (let* ([l (length input)]
               [freq (apply +. (map (cut list-ref <> index) input))]
               [bit  (if (op freq (/ l 2)) 1 0)])
          (aux (filter (λ (x) (equal? (list-ref x index) bit)) input) (+ index 1)))))
  (aux input 0))

(submit 2 (* (binlist->integer (finder2 < input)) (binlist->integer (finder2 >= input))) #f)

