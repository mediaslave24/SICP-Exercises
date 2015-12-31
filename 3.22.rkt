#lang r5rs

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '())))
  (define (set-front-ptr! p)
    (begin (set! front-ptr p)
           front-ptr))
  (define (set-rear-ptr! p)
    (begin (set! rear-ptr p)
           rear-ptr))
  (define (empty-queue?)
    (null? front-ptr))
  (define (insert-queue! i)
    (let ((new-pair (cons i '())))
      (cond [(empty-queue?)
             (set-front-ptr! new-pair)
             (set-rear-ptr! new-pair)]
            [else (set-cdr! rear-ptr new-pair)
                  (set-rear-ptr! new-pair)])))
  (define (front-queue)
    (if (empty-queue?)
      (error "FRONT called with an empty queue")
      (car front-ptr)))
  (define (delete-queue!)
    (cond [(empty-queue?)
           (error "DELETE! called with an empty queue" queue)]
          [else (set-front-ptr! (cdr front-ptr)) queue]))
  (define (dispatch m)
    (cond [(eq? m 'front-ptr) front-ptr]
          [(eq? m 'rear-ptr) rear-ptr]
          [(eq? m 'set-front-ptr!) set-front-ptr!]
          [(eq? m 'set-rear-ptr!) set-rear-ptr!]
          [(eq? m 'empty-queue?) (empty-queue?)]
          [(eq? m 'front-queue) front-queue]
          [(eq? m 'insert-queue!) insert-queue!]
          [(eq? m 'delete-queue!) delete-queue!]))
  dispatch)
