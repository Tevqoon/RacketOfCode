#lang racket

(require "common.rkt")

(define-values (numbers folds)
  (let*-values ([(numbers folds) (splitf-at (open-day 13) number?)])
    (values (let*-values ([(numbers) (list->set (pair-up numbers))]
                          [(x0 y0) (apply values (map (λ(x) (+ 1 x (modulo x 2))) (apply map max (set->list numbers))))])
              (build-list y0 (λ(y) (build-list x0 (λ(x) (set-member? numbers (list x y)))))))  
            (map (λ(x) (list (match (car x) ["x" #t] ["y" #f]) (string->number (cadr x))))
                 (map (compose (curryr string-split "=") symbol->string)
                      (cddr (remove-duplicates folds)))))))

(define (fold instr paper)
  (let-values ([(top bot) (split-at (if (car instr) (transpose paper) paper) (cadr instr))])
    ((λ(x) (if (car instr) (transpose x) x)) (map (curry map (cut or <> <>)) top (reverse (cdr bot))))))

(define (printer paper)
  (for ([line paper]) (for-each (λ(c) (display (if c "█" " "))) line) (displayln "")))

(submit 1 (count identity (flatten (fold (car folds) numbers))) #f)
(printer (foldl fold numbers folds))
