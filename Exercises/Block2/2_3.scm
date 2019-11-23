(define (flatmap proc seq)
    (fold-right append '()
        (map proc seq)))

(define (range n)
    (define (iter accum counter)
        (if (= counter -1)
            accum
            (iter (cons counter accum) (- counter 1))))
    (iter '() (- n 1)))

(define (enumerate-triples n)
    (flatmap (lambda (i) (flatmap (lambda (j) (map (lambda (k) (list k j i)
                                                   ) (range j))
                                  )  (range i))
             ) (range n))
    )


;(enumerate-triples 5)
