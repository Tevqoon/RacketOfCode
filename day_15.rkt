#lang racket

(require "common.rkt")
(require (rename-in pfds/heap/binomial
                    [map heap-map]
                    [empty? heap-empty?]
                    [filter heap-filter]
                    [remove heap-remove]
                    [andmap heap-andmap]
                    [ormap heap-ormap]))
;; Absolutely cringepilled just name your functions properly smfh

(define directions '((1 0) (-1 0) (0 1) (0 -1)))
(define input (map binstr->binlist (open-day 15 #t)))
(define test-input '((1 1 6 3 7 5 1 7 4 2)
(1 3 8 1 3 7 3 6 7 2)
(2 1 3 6 5 1 1 3 2 8)
(3 6 9 4 9 3 1 5 6 9)
(7 4 6 3 4 1 7 1 1 1)
(1 3 1 9 1 2 8 1 3 7)
(1 3 5 9 9 1 2 4 2 1)
(3 1 2 5 4 2 1 6 3 9)
(1 2 9 3 1 3 8 5 2 1)
(2 3 1 1 9 4 4 5 8 1)))

(define tiny '((1 2) (3 4)))
(define testboardboard (list (list tiny tiny) (list tiny tiny)))

(define (rowify boardrow)
  (map (curry apply append) (slice-up (apply append (transpose boardrow)) (length boardrow))))

(define (matrixfy boardboard)
  (apply append (map rowify boardboard)))

(define (magnify board)
  (matrixfy (slice-up (map (λ(v) (for/list ([y board]) (for/list ([x y]) (add1 (modulo (+ x v -1) 9)))))
                 (map (curry apply +) (cartesian-product (range 5) (range 5)))) 5)))

(define (solver board . log?)
  (let* ([len (length board)]
         [endpoint (list len len)]
         [board (make-hash (apply append
                                  (map (λ(i l) (map (λ(j x) (cons (list i j) x)) (range len) l))
                                       (range len)
                                       board)))]
         [distances (make-hash)])
    (hash-set! distances '(0 0) 0)
    (define (dijkstra to-visit)
      (unless (heap-empty? to-visit)
        (let ([x (match (find-min/max to-visit) [(cons x _) x])])
          (dijkstra
           (foldl (λ(neighbor hp)
                    (let ([alt (+ (hash-ref distances x) (hash-ref board neighbor))])
                      (if (< alt (hash-ref distances neighbor +inf.0))
                          (begin (hash-set! distances neighbor alt)
                                 (insert (cons neighbor alt) hp))
                          hp)))
                  (delete-min/max to-visit)
                  (apply neighbors x directions endpoint) 
                  )))
          ))
    (dijkstra (heap (λ(x y) (< (cdr x) (cdr y)))
                    (cons '(0 0) 0)))
    (hash-ref distances (-. endpoint '(1 1)))))

(submit 1 (solver input) #f)
 ;; Very slow, dunno what went wrong, either the heap or the hash is slow
(submit 2 (solver (magnify input)) #f)

