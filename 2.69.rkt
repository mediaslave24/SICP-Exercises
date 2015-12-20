#lang scheme

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if [leaf? tree]
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if [leaf? tree]
    (weight-leaf tree)
    (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if [null? bits]
      '()
      (let [(next-branch
              (choose-branch (car bits)
                             current-branch))]
        (if [leaf? next-branch]
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond [(= bit 0) (left-branch branch)]
        [(= bit 1) (right-branch branch)]
        [else (error "bad bit -- CHOOSE-BRANCH" bit)]))

(define (adjoin-set x set)
  (cond [(null? set) (list x)]
        [(< (weight x)
            (weight (car set))) (cons x set)]
        [else (cons (car set)
                    (adjoin-set x (cdr set)))]))

(define (make-leaf-set pairs)
  (if [null? pairs]
    '()
    (let [(pair (car pairs))]
      (adjoin-set (make-leaf (car pair)   ; symbol
                             (cadr pair)) ; frequency
                  (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (encode message tree)
  (if [null? message]
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (include a symbols)
    (cond [(null? symbols) false]
          [(eq? a (car symbols)) true]
          [else (include a (cdr symbols))]))
  (define (search tree)
    (if [leaf? tree]
      '()
      (let [(left (left-branch tree))
            (right (right-branch tree))]
        (cond [(include symbol (symbols left))
               (cons 0 (search left))]
              [(include symbol (symbols right))
               (cons 1 (search right))]
              [else (error "Wrong symbol -- " symbol)]))))
  (search tree))

(define message '(A D A B B C A))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leafs)
  (cond [(null? leafs) '()]
        [(null? (cdr leafs)) (car leafs)]
        [else (let [(left (car leafs))
                    (right (cadr leafs))
                    (other (cddr leafs))]
                (successive-merge (cons (make-code-tree left right) other)))]))

(generate-huffman-tree (list '(A 8) '(B 4) '(C 3) '(D 1) '(E 2)))