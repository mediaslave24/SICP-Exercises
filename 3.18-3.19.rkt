#lang r5rs

(define true 1)
(define false 0)

(define cycled (list 1))
(set-cdr! cycled cycled)

(define (cycle? l)
  (define (cycle-iter x)
    (cond ((null? x) false)
          ((eq? x l) true)
          (else (cycle-iter (cdr x)))))
  (if (null? l)
    false
    (cycle-iter (cdr l))))

(display (cycle? cycled))
(display "\n")
(display (cycle? (list 1 2 3)))
