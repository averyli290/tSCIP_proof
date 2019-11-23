(define (flatmap proc seq)
    (fold-right append '()
        (map proc seq)))

(define (remove f l)
    (if (null? l)
      '()
      (if (not (= (car l) f))
        (cons (car l) (remove f (cdr l)))
        (remove f (cdr l)))))


(define (permutations l)
    (if (null? (cdr l))
        (list l)
        (flatmap 
            (lambda (elt) (map (lambda (sublist) (append (list elt) sublist)) 
                               (permutations (remove elt l))))
            l)))

;(define test (list 1 2 3 4))
;(permutations test)

