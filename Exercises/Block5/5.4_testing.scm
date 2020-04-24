;; RML code for constructing '(10 20 30):

(define (get-RML) '(
    start
        (assign (reg p-1) (const 30))
        (assign (reg p-2) (op make-null))
        (assign (reg p-cont) (label start-a))
        (goto (label cons))
        start-a

        (assign (reg p-1) (const 20))
        (assign (reg p-2) (reg p-val)) 
        (assign (reg p-cont) (label start-b))
        (goto (label cons))
        start-b

        (assign (reg p-1) (const 10))
        (assign (reg p-2) (reg p-val))
        (assign (reg p-cont) (label start-c))
        (goto (label cons))
        start-c

        (assign (reg a) (reg p-val))

        (goto (label done))

    cons  ; inputs: p-1, p-2
        ;; YOUR CODE HERE

    car  ; input: p-1
        ;; YOUR CODE HERE

    cdr ; input: p-1
        ;; YOUR CODE HERE

    done



))