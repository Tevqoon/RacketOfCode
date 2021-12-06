#lang racket

(require "common.rkt")

(define input (open-day 6))

(define (stepper fish days)
  (let ([fishes (make-vector 9)])
    (define (read-fish fish)
      (for ([fish fish])
        (vector-set! fishes fish (+ 1 (vector-ref fishes fish)))))
    (read-fish fish)
    (define (aux zero-to-five six seven eight n)
      (if (= n 0)
          (+ (apply + zero-to-five) six seven eight)
          (aux (append (cdr zero-to-five) (list six))
               (+ seven (car zero-to-five))
               eight
               (car zero-to-five)
               (- n 1))))
    (aux (take (vector->list fishes) 6)
         (vector-ref fishes 6)
         (vector-ref fishes 7)
         (vector-ref fishes 8)
         days)))

(submit 1 (stepper input 80) #f)
(submit 2 (stepper input 256) #f)

