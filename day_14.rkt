#lang racket

(require "common.rkt")

(define input (open-day 14 #t))

(define-values (starting-pairs rules)
  (let*-values ([(rule-table pair-table) (values (make-hash) (make-hash))]
                [(seq rl) (split-at (open-day 14 #t) 1)]
                [(pairs) ((λ(x) (mapcar (cut list <> <>) x (cdr x))) (string->list (car seq)))])
    (for ([rule (map (curryr string-split " -> ") (cdr rl))])
      (hash-set! rule-table (string->list (car rule)) (car (string->list (cadr rule)))))
    (for ([pair pairs])
      (hash-update! pair-table pair add1 0))
    (values pair-table rule-table)))

(define (step pair-table rule-table)
  (let ([new-pairs (make-hash)])
    (hash-for-each
     pair-table
     (λ(pair c)
       (let ([inserted (hash-ref rule-table pair)])
       (hash-update! new-pairs (list (car pair) inserted)  (curry + c) 0)
       (hash-update! new-pairs (list inserted (cadr pair)) (curry + c) 0))))
    new-pairs))

(define (solver pair-table rule-table steps)
  (let ([final-pairs (repeater (curryr step rule-table) pair-table steps)]
        [counts (make-hash)])
    (hash-for-each
     final-pairs
     (λ(pair c)
       (hash-update! counts (car pair)  (curry + c) 0)
       (hash-update! counts (cadr pair) (curry + c) 0)))
    ((λ(x) (ceiling (/ (- (first x) (last x)) 2))) (sort (hash-values counts) >))))

(submit 1 (solver starting-pairs rules 10) #f)
(submit 2 (solver starting-pairs rules 40) #f)


