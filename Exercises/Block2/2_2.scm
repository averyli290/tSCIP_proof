
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
