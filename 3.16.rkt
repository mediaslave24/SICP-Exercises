#lang r5rs

(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

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
