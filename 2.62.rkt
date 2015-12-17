#lang scheme
(define (element-of-set? x set)
  (cond [(null? set) false]
        [(= x (car set)) true]
        [(< x (car set)) false]
        [else (element-of-set? x (cdr set))]))

(define (intersection-set set1 set2)
  (if (or (null? set1)
          (null? set2))
    '()
    (let [(x1 (car set1))
          (x2 (car set2))]
      (cond [(= x1 x2) (cons x1
                             (intersection-set (cdr set1)
                                               (cdr set2)))]
            [(< x1 x2) (intersection-set (cdr set1) set2)]
            [(< x2 x1) (intersection-set set1 (cdr set2))]))))

(define (adjoin-set x set)
  (cond [(null? set) (cons x '())]
        [(< x (car set)) (cons x set)]
        [(= x (car set)) set]
        [else (cons (car set)
                    (adjoin-set x (cdr set)))]))

(define (union-set set1 set2)
  (cond [(null? set1) set2]
        [(null? set2) set1]
        [(= (car set1) (car set2))
         (cons (car set1)
               (union-set (cdr set1)
                          (cdr set2)))]
        [(< (car set1)
            (car set2))
         (cons (car set1) (union-set (cdr set1) set2))]
        [else
          (cons (car set2) (union-set set1 (cdr set2)))]))

(union-set '(1 5 51 99) '(23 51 100))
