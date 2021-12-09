#lang racket

(require "common.rkt")

(define (process-line line)
  (map (compose (curry map string->list) string-split) (string-split line "|")))

(define input (map process-line (open-day 8 #t)))

(define (solver1 input)
  (count (λ(x) (member (length x) '(2 3 4 7))) (apply append (map cadr input))))

(submit 1 (solver1 input) #f)

(define (get-configuration line)
  (let ([digits (make-vector 10)])
    (define (set-pop n l . preds)
      (let-values ([(el lst) (apply pop line (cons (λ(x) (= (length x) l)) preds))])
        (vector-set! digits n el)
        (set! line lst)))
    (set-pop 1 2)
    (set-pop 4 4)
    (set-pop 7 3)
    (set-pop 8 7)
    (set-pop 3 5 (cut subset? (vector-ref digits 1) <>))
    (set-pop 9 6 (cut subset? (vector-ref digits 3) <>))
    (set-pop 0 6 (cut subset? (vector-ref digits 1) <>))
    (set-pop 6 6)
    (set-pop 5 5 (cut subset? <> (vector-ref digits 6)))
    (set-pop 2 5)
    digits))

(define (get-message configuration message)
  (digits->number (map (λ(y) (vector-memf (curry set=? y) configuration)) message)))

(define (solver2 input)
  (apply + (map (λ(x) (get-message (get-configuration (car x)) (cadr x))) input)))

(submit 2 (solver2 input) #f)

