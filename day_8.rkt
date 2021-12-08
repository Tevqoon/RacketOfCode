#lang racket

(require "common.rkt")

(define (process-line line)
  (map (compose (cut map string->list <>) string-split) (string-split line "|")))

(define input (map process-line (open-day 8 #t)))

(define (solver1 input)
  (count (λ(x) (member (length x) '(2 3 4 7)))
         (apply append (map cadr input))))

(submit 1 (solver1 input) #f)

(define (get-configuration line)
  (let ([digits (make-vector 10)])
    (define (set-pop n . preds)
      (let-values ([(el lst) (apply pop line preds)])
        (vector-set! digits n el)
        (set! line lst)))
    (set-pop 1 (length? 2))
    (set-pop 4 (length? 4))
    (set-pop 7 (length? 3))
    (set-pop 8 (length? 7))
    (set-pop 3 (length? 5) (cut subset? (vector-ref digits 1) <>))
    (set-pop 9 (length? 6) (cut subset? (vector-ref digits 3) <>))
    (set-pop 0 (length? 6) (cut subset? (vector-ref digits 1) <>))
    (set-pop 6 (length? 6))
    (set-pop 5 (length? 5) (cut subset? <> (vector-ref digits 6)))
    (set-pop 2 (length? 5))
    digits))

(define (get-message configuration message)
  (map (λ(y) (vector-memf (λ(x) (set=? x y)) configuration)) message))

(define (solver2 input)
  (apply + (map digits->number
                (map (λ(x) (get-message (get-configuration (car x)) (cadr x)))
                     input))))

(submit 2 (solver2 input) #f)

