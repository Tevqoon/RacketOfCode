#lang racket

(require "common.rkt")

(define (process-line line)
  (map (compose (cut map string->list <>) string-split) (string-split line "|")))

(define input (map process-line (open-day 8 #t)))
(define test-input (map process-line (file->lines "test_8.in")))

(define (solver1 input)
  (count (λ(x) (member (length x) '(2 3 4 7)))
         (apply append (map cadr input))))

(submit 1 (solver1 input) #f)

(define (pop lst . prds)
  "Returns the first element to satisfy all prds and the rest of the list.
   Assumes a match will be found."
  (define (aux lst acc)
    (match lst
      [(cons x xs)
       (if (andmap (λ(p) (p x)) prds)
           (values x (append (reverse acc) xs))
           (aux xs (cons x acc)))]
      [(list x) (values x '())]))
  (aux lst '()))

(define numbers '(1 4 7 8 3 9 0 6 5 2))
(define digit-c '(2 4 3 7 5 6 6 6 5 5))

(define (get-configuration line-left)
  "Takes digits as list of char lists."
  (let ([digits (make-vector 10)])
    (define (aux line-rest numbers digit-count)
      (if (empty? line-rest)
          '()
          (let-values ([(el rst) (pop line-rest (λ(x) (= (car digit-count) (length x)))
                                       (match (car numbers)
                                         [3 (cut subset? (vector-ref digits 1) <>)]
                                         [9 (cut subset? (vector-ref digits 3) <>)]
                                         [0 (cut subset? (vector-ref digits 1) <>)]
                                         [5 (cut subset? <> (vector-ref digits 6))]
                                         [n (λ(x) #t)])
                                      )])
            (vector-set! digits (car numbers) el)
            (aux rst (cdr numbers) (cdr digit-count)))))
    (aux line-left numbers digit-c)
    digits))

(define (vector-memf prd vec)
  (vector-member #t (vector-map prd vec)))

(define (get-message configuration message)
  (map (λ(y) (vector-memf (λ(x) (set=? x y)) configuration)) message))

(define (solver2 input)
  (apply + (map digits->number (map (λ(x) (get-message (get-configuration (car x)) (cadr x))) input))))

(submit 2 (solver2 input) #f)
