(define (data tree) (car tree))
(define (left-subtree tree) (cadr tree))
(define (right-subtree tree) (caddr tree))
(define (make-node entry left right)
      (list entry left right))


(define tree '(1 () (3 (2 () ()) (5 () ()))))

(define (bst-to-list-1 tree)
    (if (null? tree) '()
        (append (bst-to-list-1 (left-subtree tree)) (list (car tree)) (bst-to-list-1 (right-subtree tree)))
        )
    )

(define (bst-to-list-2 tree)
    (define (iter t accum)
        (cond ((null? t) accum)
            ((and (null? (left-subtree t)) (null? (right-subtree t))) (cons (car t) accum))
            ; if the current tree is a leaf, return accum with the value of the leaf in front
            (else (iter (left-subtree t) (cons (car t) (iter (right-subtree t) accum))))
            ; perform iter on left-subtree with an accum of what is produced by (iter right-subtree accum) and adding the value of the current tree to that
            )
        )
    (iter tree '())
    )


