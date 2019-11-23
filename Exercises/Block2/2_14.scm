(define (data tree) (car tree))
(define (left-subtree tree) (cadr tree))
(define (right-subtree tree) (caddr tree))
(define (make-node entry left right)
    (list entry left right))


(define tree '(1 () (3 (2 () ()) (5 () ()))))


(define (elt-of-set? obj set)
    (cond ((null? set) #f)
          ((= obj (car set)) #t)
          ((< obj (car set)) (elt-of-set? obj (left-subtree set)))
          (else (elt-of-set? obj (right-subtree set)))
          )
    )

(define (add-to-set obj set)
    (cond ((null? set) obj)
          ((= obj (data set)) set)
          ((> obj (data set)) (list (data set) (left-subtree set) (add-to-set obj (right-subtree set))))
          (else (list (data set) (add-to-set obj (left-subtree set)) (right-subtree set)))
          )
    )

; The running times of elt-of-set? and add-to-set are each log n because it will take at most log_2(n) time to find the element or to find the position to put the element into.
