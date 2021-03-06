(define (pi-rec n)
    (if (= n 1) 
        (/ 1.0 3)
        (+ (/ 1.0 (* (- (* 2 (+ n 1)) 1) (+ (* 2 (+ n 1)) 1))) (pi-rec (- n 1)))))

(define (pi-it n)
    (define (iter a c)
        (if (<= c 4)
            (+ a (/ 1.0 3))
            (iter (+ a (/ 1.0 (* (- c 1) (+ c 1)))) (- c 2))))
    (iter 0 (* 2 n)))
