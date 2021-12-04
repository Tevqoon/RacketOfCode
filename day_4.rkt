#lang racket

(require "common.rkt")
(require srfi/26) ; Cut macro for partial function application

(define input (open-day 4 #t))

(define numbers (map string->number (string-split (car input) ",")))

(define (board->ll board)
  (map (λ (s) (map string->number (string-split s))) (cdr board)))

(define boards (map board->ll (slice-up (cdr input) 6)))

(define (check-board board)
  (or (member '(#f #f #f #f #f) board) ; if row full
      (member '(#f #f #f #f #f) (transpose board)))) ; if column full

(define (update-board board number)
  (let ([new-board (map (cut map (λ(x) (if (equal? number x) #f x)) <>) board)])
    (if (check-board new-board)
        (* number (apply + (filter identity (flatten new-board))))
        new-board)))

(define (stepper1 boards numbers)
  (for/fold ([boards boards]
             #:result (car (filter number? boards)))
            ([n (in-list numbers)])
    #:break (ormap number? boards)
    (map (cut update-board <> n) boards)
    ))

(submit 1 (stepper1 boards numbers) #f)

(define (stepper2 boards numbers)
  (for/fold ([boards boards]
             #:result (car boards))
            ([n (in-list numbers)])
    #:break (number? (car boards))
    (if (null? (cdr boards))
        (map (cut update-board <> n) boards)
        (filter-not number? (map (cut update-board <> n) boards)))
    ))

(submit 2 (stepper2 boards numbers) #f)
