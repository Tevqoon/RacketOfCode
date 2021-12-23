#lang racket

(require "common.rkt")

(define (message->bits message)
  (flatten (map (λ(x) ((compose hexnum->binlist string->number)
                                   (string-append "#x"
                                                  (make-string 1 x))))
                            (string->list (car message)))))

(define input (message->bits (open-day 16 #t)))

(define (process-value bits)
  "Assumes the version and type have been dropped.
   Returns the value, the rest of the bits, and the total length of the value packet."
  (define (aux acc lst n)
    (match lst
      [(cons 1 xs) (aux (cons (take xs 4) acc) (drop xs 4) (add1 n))]
      [(cons 0 xs) (values ((compose binlist->integer flatten reverse) (cons (take xs 4) acc))
                           (drop xs 4)
                           (+ 6 (* 5 (add1 n))))]))
  (aux '() bits 0))

(define total-version 0)

(define (op->fun op)
  (match op
    [0 +]
    [1 *]
    [2 min]
    [3 max]
    [5 (λ(x y) (if (> x y) 1 0))]
    [6 (λ(x y) (if (< x y) 1 0))]
    [7 (λ(x y) (if (= x y) 1 0))]))

(define (process-packet bits)
  (let*-values ([(vrsn tail) (split-at bits 3)]
                [(type tail) (split-at tail 3)]
                [(vrsn type) (values (binlist->integer vrsn) (binlist->integer type))] )
    (match type
      [4 (let-values ([(val tail n) (process-value tail)])
           (set! total-version (+ vrsn total-version))
           (values val tail n))]
      [op #:when (= 0 (first tail))
          (let*-values ([(total-length tail) (split-at (rest tail) 15)]
                        [(total-length) (binlist->integer total-length)])
            (define (aux acc lst n)
              (if (= n total-length)
                  (values (reverse acc) lst)
                  (let-values ([(val tail m) (process-packet lst)])
                    (aux (cons val acc) tail (+ n m)))))
            (let-values ([(vals tail) (aux '() tail 0)])
              (set! total-version (+ vrsn total-version))
              (values (cons (op->fun op) vals) tail (+ 7 15 total-length))))]
      [op #:when (= 1 (first tail))
          (let*-values ([(num-packets tail) (split-at (rest tail) 11)]
                        [(num-packets) (binlist->integer num-packets)])
            (define (aux acc lst n num)
              (if (= num num-packets)
                  (values (reverse acc) lst n)
                  (let-values ([(val tail m) (process-packet lst)])
                    (aux (cons val acc) tail (+ m n) (add1 num)))))
            (let-values ([(vals tail n) (aux '() tail 0 0)])
              (set! total-version (+ vrsn total-version))
              (values (cons (op->fun op) vals) tail (+ 7 11 n))))])))

(let-values ([(val tail n) (process-packet input)])
  (eval val))
total-version
