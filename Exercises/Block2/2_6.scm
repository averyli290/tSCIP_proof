(define (subsets l)
    (define (recur choose chosen)
        (if (null? choose)
          (list chosen)
          (append (recur (cdr choose) (append chosen (list (car choose)))) (recur (cdr choose) chosen))))
    (recur l '())
    )

;(subsets (list 1 2 3))
