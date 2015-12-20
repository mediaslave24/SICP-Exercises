#lang scheme

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond [(null? set) false]
        [(= x (entry set)) true]
        [(< x (entry set))
         (element-of-set? x (left-branch set))]
        [(> x (entry set))
         (element-of-set? x (right-branch set))]))

(define (adjoin-set x set)
  (cond [(null? set) (make-tree x '() '())]
        [(= x (entry set)) set]
        [(< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set))]
        [(> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set)))]))

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (define (partial-tree elts n)
    (if (= n 0)
      (cons '() elts)
      (let [(left-size (quotient (- n 1) 2))]
        (let [(left-result (partial-tree elts left-size))]
          (let [(left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1)))]
            (let [(this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size))]
              (let [(right-tree (car right-result))
                    (remaining-elts (cdr right-result))]
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
  (car (partial-tree elements (length elements))))

(define (union-set set1 set2)
  (define (merge list1 list2)
    (cond [(null? list1) list2]
          [(null? list2) list1]
          [else (let [(x1 (car list1))
                      (x2 (car list2))]
                  (cond [(= x1 x2) (cons x1 (merge (cdr list1)
                                                   (cdr list2)))]
                        [(< x1 x2) (cons x1 (merge (cdr list1)
                                                   list2))]
                        [(> x1 x2) (cons x2 (merge list1
                                                   (cdr list2)))]))]))
  (list->tree (merge (tree->list set1)
                     (tree->list set2))))

(define (intersection-set set1 set2)
  (define (intersect list1 list2)
    (if [or (null? list1)
            (null? list2)]
      '()
      (let [(x1 (car list1))
            (x2 (car list2))]
        (cond [(= x1 x2) (cons x1 (intersect (cdr list1)
                                             (cdr list2)))]
              [(< x1 x2) (intersect (cdr list1) list2)]
              [(> x1 x2) (intersect list1 (cdr list2))]))))
  (list->tree (intersect (tree->list set1)
                         (tree->list set2))))

(define set1
  (make-tree 10
             (make-tree 5
                        (make-tree 2
                                   (make-tree 1 '() '())
                                   (make-tree 3 '() '()))
                        (make-tree 8
                                   (make-tree 7 '() '())
                                   (make-tree 9 '() '())))
             (make-tree 20
                        (make-tree 15
                                   (make-tree 13
                                              (make-tree 11 '() '())
                                              (make-tree 14 '() '()))
                                   (make-tree 18
                                              (make-tree 16 '() '())
                                              (make-tree 19 '() '())))
                        (make-tree 25
                                   '()
                                   '()))))

(define set2
  (make-tree 18
             (make-tree 10
                        (make-tree 5 '() '())
                        (make-tree 12 '() '()))
             (make-tree 22
                        (make-tree 20 '() '())
                        (make-tree 23 '() '()))))

(union-set set1 set2)
(intersection-set set1 set2)
