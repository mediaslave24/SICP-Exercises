#lang scheme

(define acc false)

(define (f n)
  (if (eq? acc 0)
    acc
    (begin (set! acc n) acc)))

(+ (f 0) (f 1))
#| (+ (f 1) (f 0)) |#
