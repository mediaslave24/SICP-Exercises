#lang scheme

(define (make-account init password)
  (lambda (pass action num)
    (if (eq? pass password)
      (cond [(eq? action 'withdraw)
             (begin (set! init (- init num)) init)]
            [(eq? action 'deposit)
             (begin (set! init (+ init num)) init)]
            [else error "Unknown action"])
      "Wrong password")))

(define (make-joint acc password new-password)
  (lambda (pass action num)
    (if (eq? pass new-password)
      (acc password action num)
      (acc pass action num))))

(define acc (make-account 100 'pass))
