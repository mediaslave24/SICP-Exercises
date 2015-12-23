#lang scheme

(define (make-account init password)
  (define (call-the-cops) "Cops!")
  (define calls 0)
  (lambda (entered-password action number)
    (if [eq? entered-password password]
      (cond [(eq? action 'withdraw) (begin (set! init (- init number)) init)]
            [(eq? action 'deposit) (begin (set! init (+ init number)) init)])
      (if [< calls 7]
        (begin (set! calls (+ calls 1)) "Invalid password")
        (call-the-cops)))))

(define acc (make-account 100 'pass))

(acc 'hey 'withdraw 50)
(acc 'hey 'withdraw 50)
(acc 'hey 'withdraw 50)
(acc 'hey 'withdraw 50)
(acc 'hey 'withdraw 50)
(acc 'hey 'withdraw 50)
(acc 'hey 'withdraw 50)
(acc 'hey 'withdraw 50)
(acc 'hey 'withdraw 50)
(acc 'pass 'withdraw 45)
(acc 'pass 'deposit 45)
