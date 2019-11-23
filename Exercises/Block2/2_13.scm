(define (flatmap proc seq)
    (fold-right append '()
        (map proc seq)))

(define (elt-of-set? obj set)
    (if (null? set)
        #f
        (if (= (car set) obj)
            #t
            (elt-of-set? obj (cdr set))))
    )

(define (add-to-set obj set)
    (cond ((null? set) (list obj))
          ((< obj (car set)) (cons obj set))
          (else (cons (car set) (add-to-set obj (cdr set))))
          )
    )

(define (union set-1 set-2)
    (define (iter accum s-1 s-2)
        (cond ((null? s-1) (append accum s-2))
              ((null? s-2) (append accum s-1))
              ((< (car s-1) (car s-2)) (iter (append accum (list (car s-1))) (cdr s-1) s-2))
              (else (iter (append accum (list (car s-2))) s-1 (cdr s-2)))
              )
        )
    (iter '() set-1 set-2)
    )

(define (intersection set-1 set-2)
    (define (iter accum s-1 s-2)
        (cond ((or (null? s-1) (null? s-2)) accum)
              ((< (car s-1) (car s-2)) (iter accum (cdr s-1) s-2))
              ((> (car s-1) (car s-2)) (iter accum s-1 (cdr s-2)))
              (else (iter (append accum (list (car s-2))) (cdr s-1) (cdr s-2)))
              )
        )
    (iter '() set-1 set-2)
    )

; elt-of-set runs in O(n) time, it needs to check at maximum n elements in the list
; add-to-set runs in O(n) time, it needs to check a maximum n elements in the list to find the position to insert the element in the correct position
; union takes 2n time, it only needs to compare two elements at a time cdring down the list which has the smallar number, and since each list has n elements each, it will take at maximum 2n time to run union
; intersection takes 2n time, for the same reason as union, but to make sure that they exist in both sets


