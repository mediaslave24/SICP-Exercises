#lang scheme

(define (count-pairs x)
  (define (present? collection object)
    (cond ((null? collection) false)
          ((eq? (car collection) object) true)
          (else (present? (cdr collection) object))))
  (define (append-uniq collection object)
    (if (present? collection object)
      collection
      (cons object collection)))
  (define (uniq-pairs x acc)
    (if (not (pair? x))
      acc
      (uniq-pairs (car x)
                  (uniq-pairs (cdr x) (append-uniq acc x)))))
  (length (uniq-pairs x '())))

(display (count-pairs (list 1 2 3)))
(display "\n")

(define a (cons 1 '()))
(define b (cons 2 a))
(define c (cons a b))
(display (count-pairs c))
(display "\n")

(define d (cons 1 '()))
(define e (cons d d))
(define f (cons e e))
(display (count-pairs f))
