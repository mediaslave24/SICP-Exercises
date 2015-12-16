#lang scheme
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (intersection-set set1 set2)
  (if [or (null? set1) (null? set2)]
    '()
    (let [(res (intersection-set (cdr set1) set2))]
      (if (and (element-of-set? (car set1) set2)
               (not (element-of-set? (car set1) (cdr set1))))
          (cons (car set1) res)
          res))))

(define (union-set set1 set2)
  (cond [(and (null? set1)
              (null? set2)) '()]
        [(null? set1) (union-set set2 '())]
        [else (let [(el (car set1))
                    (rem (cdr set1))]
                (if (or (element-of-set? el rem)
                        (element-of-set? el set2))
                  (union-set rem set2)
                  (cons el (union-set rem set2))))]))

(intersection-set '(1 2 1 2 1 2 1 2 2 2 2 1 3 3 3 9 9 8 7 6) '(6 2 2 2 2 6))
(union-set '(1 9 9 9 3 3 2 2 5 5 5 6)
           '(9 10 11 1 10 10 10 10 10))
