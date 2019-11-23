(define (depth t)
    (cond ((null? (cdr t)) 0)
          ((pair? t) (max (+ 1 (depth (cadr t))) (depth (cdr t)))) 
          (else (depth (cdr t)))
          )
    )

;(define test '(1 (2) (3 (4)) (5 (6 (7)))))
;(depth test)
