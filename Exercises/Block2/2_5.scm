
(define (flatmap proc seq)
    (fold-right append '()
        (map proc seq)))

(define (range n)
    (define (iter accum counter)
        (if (= counter n)
            accum
            (iter (cons counter accum) (+ 1 counter))))
    (iter '() 0))

(define (enumerate-pairs n)
    (flatmap (lambda (i) (map (lambda (j) (list i j)) (range n))) (range n))
    )

(define (my-filter pred l)
    (define (iter accum l)
        (cond ((null? l) accum)
              ((pred (car l)) (iter (cons (car l) accum) (cdr l)))
              (else (iter accum (cdr l)))
            )
        )
    (reverse (iter '() l))
    )


(define (f n)
    (fold-right + 0
                (map (lambda (p) (* (car p) (cadr p)))
                     (filter (lambda (p) (= (+ (car p) (cadr p)) n)) (enumerate-pairs n)))))

;(f 3)
