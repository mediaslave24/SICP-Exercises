#lang r5rs

(define cycled (list 1))
(set-cdr! cycled cycled)

(define (cycle? l)
  (define (cycle-iter x)
    (cond ((null? x) 0)
          ((eq? x l) 1)
          (else (cycle-iter (cdr x)))))
  (cycle-iter (cdr l)))

(display (cycle? cycled))
(display "\n")
(display (cycle? (list 1 2 3)))
