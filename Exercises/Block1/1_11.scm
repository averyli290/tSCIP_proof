(define (square n)
    (* n n))

(define (fast-exp b n)
    (cond ((= n 1) b)
        ((even? n) (square (fast-exp b (/ n 2))))
        ((even? (- n 1)) (* b (fast-exp b (- n 1)))))) 
