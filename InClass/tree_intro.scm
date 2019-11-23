(define x (cons 1 (cons (cons 4 (cons 5 '())) (cons 3 '()))))
(define y (list 1 (list 4 5) 3))
(define z '(1 (4 5) 3))
z

(define (count-leaves t) 
    (cond ((null? t) 0)
          ((not (pair? t)) 1) 
          (else (+ (count-leaves (car t)) (count-leaves (cdr t))))
          )
    )


(count-leaves z)


