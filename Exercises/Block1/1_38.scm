(define (length-via-acc l)
    (accumulate 0 (lambda (x y) (+ y 1)) l))

(define (map-via-acc l f)
    (accumulate '() (lambda (x y) (cons (f x) y)) l))

(define (append-via-acc l1 l2)
    (accumulate l2 (lambda (x y) (cons x y)) l1))

; (define test1 (list 1 2 3 4 5))
; (define test2 (list 6 7 8 9 10))
; (length-via-acc test1)
; (map-via-acc test1 (lambda (x) (* x x)))
; (append-via-acc test1 test2)
