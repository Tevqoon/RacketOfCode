#lang racket

(require "common.rkt")

(define input (flatten (map (λ(x) ((compose hexnum->binlist string->number)
                                   (string-append "#x"
                                                  (make-string 1 x))))
                            (string->list (car (open-day 16 #t))))))

input
