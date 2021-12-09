#lang racket

(require srfi/26) ; Cut macro for partial function application

;; Administration

(define (remove-unquotes symb)
  "If stuff is separated by commas, remove them."
  (if (or (number? symb) (symbol? symb))
      symb
      (cadr symb)))

(define (open-day n [lines? #f])
  "Opens the n-th day's input file as a list.
   If it doesn't exist, pull it with the aoc package."
  (let [(day-filename (format "day_~a.in" n))]
    (unless (file-exists? day-filename)
      (system (format "raco aoc -d ~a > ~a" n day-filename)))
    (if lines?
        (file->lines day-filename)
        (map remove-unquotes (file->list day-filename)))))

(define (submit n val [submit? #f])
  (if submit?
      (system (format "raco aoc -a ~a ~a" n val))
      val))

;; Mapping and listshit

(define (mapcar f . xss)
  "A mufch better mapcar stolen from Muf."
  (define (aux acc . xss)
    (if (ormap empty? xss)
        (reverse acc)
        (apply aux (cons (apply f (map car xss)) acc) (map cdr xss))))
  (apply aux '() xss))

(define (slice-up lst n)
  "Slices up a list into sublists length n. Excess elements ignored."
  (define (aux lst acc)
    (if (< (length lst) n)
        (reverse acc)
        (let-values ([(hd tl) (split-at lst n)])
          (aux tl (cons hd acc)))))
    (aux lst '()))

(define (pair-up lst)
  (slice-up lst 2))

(define (transpose xss)
  "Just transpose pepega."
  (apply map list xss))

(define (zip . lists)
  "Huh i guess zipping is basically transposing"
  (apply mapcar list lists))

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

(define (pad l* padsymb)
  (let ([l (+ 2 (length (car l*)))])
    (append (list (build-list l (const padsymb)))
            (map (λ(x) (append (list padsymb) x (list padsymb))) l*)
            (list (build-list l (const padsymb))))))

;; Vector barf

(define (vector-memf prd vec)
  (vector-member #t (vector-map prd vec)))

;; Tuples are of form (x1 x2 . x3).

(define (map. proc . tuples)
  "Applies proc to tuples elementwise.
   Assumes same length tuples and at least one given element. Also works on lists."
  (define (aux tuples)
    (cond [(pair? (car tuples))
           (cons (apply proc (map car tuples))
              (aux (map cdr tuples)))]
          [(null? (car tuples)) null]
          [else (apply proc tuples)]))
  (aux tuples))

(define (andmap. prd . tuples)
  (let ([vals (apply map. prd tuples)])
    (define (aux rst)
      (match rst
        [(cons x xs) #:when (list? xs) (and x (aux xs))]
        [(cons x y) (and x y)]
        [x x]))
    (aux vals)))

;; Some basic tuple operations

(define (+. . tuples)
  (apply map. + tuples))

(define (-. . tuples)
  (apply map. - tuples))

(define (*. . tuples)
  (apply map. * tuples))

(define (/. . tuples)
  (apply map. / tuples))

(define (<. . tuples)
  (apply andmap. < tuples))

(define (>. . tuples)
  (apply andmap. > tuples))

(define (mult2 tuple)
  "Multiply first two elements of a tuple/list/whatever."
  (if (pair? (cdr tuple))
      (* (car tuple) (cadr tuple))
      (* (car tuple) (cdr tuple))))

;; Number

(define (binstr->binlist binstr)
  (map (cut - <> 48)
       (map char->integer (string->list binstr))))

(define (binlist->binstr binlist)
  (list->string (map integer->char
                     (map (cut + <> 48) binlist))))

(define (binstr->integer binstr)
  (string->number (format "#b~a" binstr)))

(define (binlist->integer binlist)
  (binstr->integer (binlist->binstr binlist)))

(define (digits->number digits)
  (define (aux acc lst ex)
    (if (empty? lst)
        acc
        (aux (+ acc (* (car lst) (expt 10 ex))) (cdr lst) (add1 ex))))
  (aux 0 (reverse digits) 0))

(define (number->list num)
  (map (lambda (c) (- (char->integer c) (char->integer #\0)))
       (string->list
        (number->string num))))


;; Providing

(provide (all-defined-out) cut)
