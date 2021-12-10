#lang racket

(require "common.rkt")

(define input (map string->list (open-day 10 #t)))

(define opens '((#\( #\)) (#\[ #\]) (#\{ #\}) (#\< #\>)))
(define error-scores '((#\) 3) (#\] 57) (#\} 1197) (#\> 25137)))
(define completion-scores '((#\) 1) (#\] 2) (#\} 3) (#\> 4)))

(define (checker line)
  (define (aux line stack)
    (match line
      [(cons x xs) #:when (assoc x opens)
                   (aux xs (cons (cadr (assoc x opens)) stack))]
      [(cons x xs) #:when (equal? x (car stack))
                   (aux xs (cdr stack))]
      [(cons x xs) (cadr (assoc x error-scores))]
      [else stack]))
  (aux line '()))

(define (completion-score rest-line)
  (foldl (λ(char score) (+ (* 5 score) (cadr (assoc char completion-scores)))) 0 rest-line))

(define-values (corrupted incompletes) (partition number? (map checker input)))

(submit 1 (apply + corrupted) #f)

(submit 2 (list-ref (sort (map completion-score incompletes) <)
                    (quotient (length incompletes) 2)) #f)
