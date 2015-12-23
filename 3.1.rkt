#lang scheme

(define (make-accumulator init)
  (define acc init)
  (lambda (num) (begin (set! acc (+ acc num)) acc)))

(define A (make-accumulator 5))
(A 10)
