#lang racket

(require "common.rkt")
(require srfi/26) ; Cut macro for partial function application

(define input (open-day 4 #t))

(define numbers (map string->number (string-split (car input) ",")))

(define (board->ll board)
  (map (λ(s) (map string->number (string-split s))) (cdr board)))

(define boards (map board->ll (slice-up (cdr input) 6)))

(define (check-board board)
  (or (member '(#f #f #f #f #f) board) ; if row full
      (member '(#f #f #f #f #f) (transpose board)))) ; if column full

(define (update-board board number)
  (let ([new-board (map (cut map (λ(x) (if (equal? number x) #f x)) <>) board)])
    (if (check-board new-board)
        (* number (apply + (filter identity (flatten new-board))))
        new-board)))

(define (stepper break? boards numbers)
  (for/fold ([boards boards]
             #:result (car (filter number? boards)))
            ([n (in-list numbers)])
    #:break (break? boards)
    (map (cut update-board <> n) (filter-not number? boards))))

(submit 1 (stepper (cut ormap  number? <>) boards numbers) #f) ; Stop when there is one number
(submit 2 (stepper (cut andmap number? <>) boards numbers) #f) ; Stop when there is only one number
