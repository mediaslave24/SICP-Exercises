#lang scheme

(define (lookup k set)
  (if [null? set]
    false
    (let [(x (entry set))]
      (cond [(= k (key x)) x]
            [(< k (key x))
             (lookup k (left-branch set))]
            [(> k (key x))
             (lookup k (right-branch set))]))))
