(define (flatmap proc seq)
    (fold-right append '()
        (map proc seq)))

(define (range n)
    (define (iter accum counter)
        (if (= counter n)
            accum
            (iter (cons counter accum) (+ 1 counter))))
    (iter '() 0))

(define (queens board-size)
    (define empty-board '())
    (define (adjoin-position r k other-queens)
        (cons (cons r k) other-queens))
    (define (safe? k positions)
        (let ((q (car positions)))
          
            (begin 
              (fold-right (lambda (bool1 bool2) (and bool1 bool2)) #t 
                        (map (lambda (pos) (if (and (not (= (car pos) (car q)))
                                                    (not (= (abs (- (car pos) (car q))) (abs (- (cdr pos) (cdr q))))))
                                                #t
                                                #f))
                             (cdr positions)))
              )
            )
          
        )

    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter
                (lambda (positions) (safe? k positions))
                    (flatmap
                    (lambda (rest-of-queens)
                        (map (lambda (new-row)
                            (adjoin-position new-row k rest-of-queens))
                        (map 1+ (range board-size))))
                    (queen-cols (- k 1))))))

    (queen-cols board-size))


(define (sizeof l)
    (if (null? l) 0 (+ 1 (sizeof (cdr l)))))

(sizeof (queens 8))



