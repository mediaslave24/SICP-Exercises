#lang r5rs
(define (mystery x)
  (define (loop x y)
    (if (null? x)
      y
      (let ((temp (cdr x)))
        (set-cdr! x y)
        (loop temp x))))
  (loop x '()))

(define w (list 'a 'b 'c 'd))

(define v (mystery w))

(display w)
(display v)
