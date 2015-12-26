#lang scheme

(define (random-in-range low high)
  (let [(range (- high low))]
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond [(= trials-remaining 0)
           (/ trials-passed trials)]
          [(experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1))]
          [else
            (iter (- trials-remaining 1) trials-passed)]))
  (iter trials 0))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (* (monte-carlo trials
                  (lambda ()
                    (p (random-in-range x1 x2)
                       (random-in-range y1 y2))))
     (* (- x2 x1)
        (- y2 y1))))

(define (p x y) (< (+ (sqr (- x 5))
                      (sqr (- y 7)))
                   (sqr 3)))

(estimate-integral p 2 8 4 10 999999)
