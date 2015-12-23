#lang scheme

(define (make-monitored proc)
  (define calls 0)
  (lambda args
    (cond [(null? args) (proc)]
          [(eq? (car args) 'how-many-calls?) calls]
          [(eq? (car args) 'reset-count) (set! calls 0)]
          [else (begin (set! calls (+ calls 1)) (apply proc args))])))

(define s (make-monitored sqr))

(s 2)
(s 4)
(s 8)
(s 'how-many-calls?)
(s 'reset-count)
(s 'how-many-calls?)
