(define (let-as-lambda a b)
    ((lambda (result) (if (< 0 result) result 0))
        (+ (/ 1 a) (/ 1 b)))


(define a 4)

((lambda (a b) (+ a b)) 1 (+ a 10))
