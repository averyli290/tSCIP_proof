(define test (list 1 2 3))
(let ((temp test))
    (define (iter l)
        (if (null? l)
            0
            (begin (newline) (display test) (newline) (display l) (iter (cdr l)))))
    (iter temp)
  )
