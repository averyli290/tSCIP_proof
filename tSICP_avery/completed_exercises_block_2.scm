

;;; BEGIN:: 2.1

(define (enumerate tree)
    (cond ((null? tree) '())
        ((pair? tree) (append (enumerate (car tree)) (enumerate (cdr tree))))
        (else (list tree))))

(define (square-leaves tree)
    (reduce + 0 (map (lambda (x) (* x x))
                     (enumerate tree))
            )
    )

;(square-leaves (list 1 2 (list 3)))

;;; END: 2.1


;;; BEGIN: 2.2

(define (flatmap proc seq)
    (fold-right append '()
        (map proc seq)))

(define (range n)
    (define (iter accum counter)
        (if (= counter n)
            accum
            (iter (cons counter accum) (+ 1 counter))))
    (iter '() 0))

(define (enumerate-pairs n)
    (flatmap (lambda (i) (map (lambda (j) (list i j)) (range n))) (range n))
    )

;;; END: 2.2


;;; BEGIN: 2.3

(define (range n)
    (define (iter accum counter)
        (if (= counter -1)
            accum
            (iter (cons counter accum) (- counter 1))))
    (iter '() (- n 1)))

(define (enumerate-triples n)
    (flatmap (lambda (i) (flatmap (lambda (j) (map (lambda (k) (list k j i)
                                                   ) (range j))
                                  )  (range i))
             ) (range n))
    )


;(enumerate-triples 5)

;;; END: 2.3


;;; BEGIN: 2.4

(define (my-filter pred l)
    (define (iter accum l)
        (cond ((null? l) accum)
              ((pred (car l)) (iter (cons (car l) accum) (cdr l)))
              (else (iter accum (cdr l)))
            )
        )
    (reverse (iter '() l))
    )

;(my-filter (lambda (x) (> x 0)) (list -1 2 3 -4))

;;; END: 2.4


;;; BEGIN: 2.5

(define (range n)
    (define (iter accum counter)
        (if (= counter n)
            accum
            (iter (cons counter accum) (+ 1 counter))))
    (iter '() 0))

(define (enumerate-pairs n)
    (flatmap (lambda (i) (map (lambda (j) (list i j)) (range n))) (range n))
    )

(define (my-filter pred l)
    (define (iter accum l)
        (cond ((null? l) accum)
              ((pred (car l)) (iter (cons (car l) accum) (cdr l)))
              (else (iter accum (cdr l)))
            )
        )
    (reverse (iter '() l))
    )


(define (f n)
    (fold-right + 0
                (map (lambda (p) (* (car p) (cadr p)))
                     (filter (lambda (p) (= (+ (car p) (cadr p)) n)) (enumerate-pairs n)))))

;(f 3)

;;; END: 2.5


;;; BEGIN: 2.6

(define (subsets l)
    (define (recur choose chosen)
        (if (null? choose)
          (list chosen)
          (append (recur (cdr choose) (append chosen (list (car choose)))) (recur (cdr choose) chosen))))
    (recur l '())
    )

;(subsets (list 1 2 3))

;;; END: 2.6


;;; BEGIN: 2.7

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



;;; END: 2.7


;;; BEGIN: 2.8

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


;;; END: 2.8


;;; BEGIN: 2.9

;;; END: 2.9


;;; BEGIN: 2.10

;;; END: 2.10


;;; BEGIN: 2.11

(define (elt-of-set? obj set)
    (if (null? set)
        #f
        (if (= (car set) obj)
            #t
            (elt-of-set? obj (cdr set))))
    )

(define (add-to-set obj set)
    (if (not (elt-of-set? obj set))
        (cons obj set)
        set
        )
    )

(define (union set-1 set-2)
    (fold-right (lambda (elt lst) (if (not (elt-of-set? elt lst))
                                    (append lst (list elt))
                                    lst))
                set-1
                set-2))


(define (intersection set-1 set-2)
    (filter (lambda (elt1) 
                (fold-right
                        (lambda (arg1 arg2) (or arg1 arg2))
                        #f
                        (map (lambda (elt2) (= elt1 elt2))
                             set-2)))
            set-1))

;;; END: 2.11


;;; BEGIN: 2.12

; elt-of-set runs in O(n) time, it needs to check at maximum n elements in the list
; add-to-set runs in O(n) time, it needs to check a maximum n elements in the list to make sure there will not be a repeat
; union takes O(n^2) time, it needs to check at maximum n elements of set-2 for each of the n elements of set-1 to make sure there are no repeats
; intersection takes O(n^2) time, for the same reason as union, but to make sure that they exist in both sets


;;; END: 2.12


;;; BEGIN: 2.13

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


;;; END: 2.13


;;; BEGIN: 2.14

(define (data tree) (car tree))
(define (left-subtree tree) (cadr tree))
(define (right-subtree tree) (caddr tree))
(define (make-node entry left right)
    (list entry left right))


;(define test-tree '(1 () (3 (2 () ()) (5 () ()))))


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


;;; END: 2.14


;;; BEGIN: 2.15

(define (data tree) (car tree))
(define (left-subtree tree) (cadr tree))
(define (right-subtree tree) (caddr tree))
(define (make-node entry left right)
      (list entry left right))


;(define test-tree '(1 () (3 (2 () ()) (5 () ()))))

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


;;; END: 2.15


;;; BEGIN: 2.16

; bst-to-list-1 takes nlog(n) time because it takes log(n) time to reach each node and append takes linear time
; bst-to-list-2 takes n time because it takes a total of n steps to get to every single node in the tree and there are no operations that would affect the time complexity

;;; END: 2.16


;;; BEGIN: 2.17

(define (make-node x y z) (list x y z))

(define (list->tree elements)
    (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts) ; Base Case: adds an empty list when there are no further elements to be added in current partial tree
            (let ((left-size (quotient (- n 1) 2))) ; Gets the amount of elements to be placed in the left partial tree of the current partial tree
                (let ((left-result (partial-tree elts left-size))) ; Creates left-result, which is made up of a left partial tree as the first element and the remaining elemnts to place in the right partial tree of the current partial tree
                    (let ((left-tree (car left-result)) ; Creates variables for the left partial tree of the current partial tree, the remaining elements to be put in the right partial tree, and the number of elements to be put in the right partial tree, however, we add one to the left-size because the middle number is going to be the entry in our current node
                          (non-left-elts (cdr left-result))
                          (right-size (- n (+ left-size 1))))
                        (let ((this-entry (car non-left-elts)) ; Assigns a variable to contain the value that the current node will have and also creates right-result containing a list which is the partial tree of the remaining elements that aren't in the left partial tree or is the current node value
                              (right-result (partial-tree (cdr non-left-elts)
                                                                right-size)))
                           (let ((right-tree (car right-result)) ; Assigns right-tree to be the car of right-result, because the first element of right-result is the right-tree and the remaining elements is an empty list (Probably? or one element left)
                                 (remaining-elts (cdr right-result)))
                              (cons (make-node this-entry left-tree right-tree)
                                 remaining-elts)))))))) ; HERE, We create the partial tree by first creating the node and then adding on any remaining elements from the right partial tree. This will be what we return in the end.

; This creates a balanced subtree because it will divide the list of elements in half each time.

;(list->tree '(1 2 3 4 5 6))

;;; END: 2.17


;;; BEGIN: 2.18

(define (make-rat x y) (cons x y))
(define (numer rat) (car rat))
(define (denom rat) (cdr rat))

;;; END: 2.18


;;; BEGIN: 2.19

(define (make-rat x y)
    (cond ((and (< x 0) (> y 0)) (cons (/ (abs x) (gcd x y)) (* (/ y (gcd x y)) -1)))
          ((and (> x 0) (< y 0)) (cons (/ x (gcd x y)) (* (/ y (gcd x y)))))
          (else (cons (/ x (gcd x y)) (/ y (gcd x y))))
        )
    )

;;; END: 2.19


;;; BEGIN: 2.20

(define (numer rat) (car rat))
(define (denom rat) (cdr rat))

(define (make-rat x y)
    (cond ((and (< x 0) (> y 0)) (cons (/ (abs x) (gcd x y)) (* (/ y (gcd x y)) -1)))
          ((and (> x 0) (< y 0)) (cons (/ x (gcd x y)) (* (/ y (gcd x y)))))
          (else (cons (/ (abs x) (gcd x y)) (/ (abs y) (gcd x y))))))


(define (add-rat rat1 rat2)
    (make-rat (+ (* (numer rat1) (denom rat2)) (* (numer rat2) (denom rat1))) (* (denom rat1) (denom rat2))))

(define (sub-rat rat1 rat2)
    (add-rat rat1 (make-rat (* (numer rat2) -1) (* (denom rat2) -1))))

(define (mul-rat rat1 rat2)
    (make-rat (* (numer rat1) (numer rat2)) (* (denom rat1) (denom rat2))))

(define (div-rat rat1 rat2)
    (make-rat (* (numer rat1) (denom rat2)) (* (denom rat1) (numer rat2))))

(define (eq-rat? rat1 rat2)
    (and (= (numer rat1) (numer rat2)) (= (denom rat1) (denom rat2))))

(define (print-rat rat)
    (begin (display (numer rat)) (display "/") (display (denom rat))))

;;; END: 2.20


;;; BEGIN: 2.21

(define (memq? sym li)
    (cond ((null? li) #f)
          ((eq? sym (car li)) #t)
          (else (memq? sym (cdr li)))))

;;; END: 2.21


;;; BEGIN: 2.22

(a b c)
((george))
(y1 . y2)
y1
#f
#f
#t


;;; END: 2.22

;;; BEGIN: 2.23

; ''mooooooooo! first evalutes to a symbol 'moooooooo!, which may evaluate to a list of character, whose first character is '.

;;; END: 2.23


;;; BEGIN: 2.24

(define (make-from-real-imag a b) (cons a b))
(define (real-part z) (car z))
(define (imag-part z) (cdr z))

;;; END: 2.24


;;; BEGIN: 2.25

(define (make-from-real-imag a b) (cons a b))
(define (real-part z) (car z))
(define (imag-part z) (cdr z))

(define (make-from-mag-ang r theta)
    (make-from-real-imag (* r (cos theta)) (* r (sin theta))))

(define (magnitude z)
    (sqrt (+ (* (real-part z) (real-part z)) (* (imag-part z) (imag-part z)))))

(define (angle z) 
    (atan (real-part z) (imag-part z)))

;;; END: 2.25


;;; BEGIN: 2.26

(define (make-from-real-imag a b) (cons a b))
(define (real-part z) (car z))
(define (imag-part z) (cdr z))

(define (make-from-mag-ang r theta)
    (make-from-real-imag (* r (cos theta)) (* r (sin theta))))

(define (magnitude z)
    (sqrt (+ (* (real-part z) (real-part z)) (* (imag-part z) (imag-part z)))))

(define (angle z) 
    (atan (real-part z) (imag-part z)))

(define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))

;;; END: 2.26


;;; BEGIN: 2.27

(define (make-from-mag-ang a b) (cons a b))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (real-part z) (* (car z) (cos (cdr z))))
(define (imag-part z) (* (car z) (sin (cdr z))))
(define (make-from-real-imag a b) (make-from-mag-ang (sqrt (+ (* a a) (* b b))) (atan a b)))

(define (add-complex z1 z2)
      (make-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
      (make-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
      (make-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
      (make-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))

;(define test1 (make-from-mag-ang 1 (/ 3.1415 2)))
;(define test2 (make-from-mag-ang 1 3.1415))

;(add-complex test1 test2)
;(sub-complex test1 test2)
;(mul-complex test1 test2)
;(div-complex test1 test2)

;;; END: 2.27


;;; BEGIN: 2.28


(define (set-tag tag object) (cons tag object))
(define (get-tag tagged-object) (car tagged-object))
(define (get-contents tagged-object) (cdr tagged-object))
(define (rectangular? tagged-object) (eq? 'rect (get-tag tagged-object)))
(define (polar? tagged-object) (eq? 'polar (get-tag tagged-object)))

(define (real-part-rect z) (car (get-contents z)))
(define (imag-part-rect z) (cdr (get-contents z)))
(define (real-part-polar z) (* (car get-contents z) (cos (cdr (get-contents z)))))
(define (imag-part-polar z) (* (car get-contents z) (sin (cdr (get-contents z)))))

;;; END: 2.28


;;; BEGIN: 2.29

(define (set-tag tag object) (cons tag object))
(define (get-tag tagged-object) (car tagged-object))
(define (get-contents tagged-object) (cdr tagged-object))
(define (rectangular? tagged-object) (eq? 'rect (get-tag tagged-object)))
(define (polar? tagged-object) (eq? 'polar (get-tag tagged-object)))

(define (real-part-rect z) (car (get-contents z)))
(define (imag-part-rect z) (cdr (get-contents z)))
(define (real-part-polar z) (* (car (get-contents z)) (cos (cdr (get-contents z)))))
(define (imag-part-polar z) (* (car (get-contents z)) (sin (cdr (get-contents z)))))

(define (real-part z)
    (if (rectangular? z)
        (real-part-rect z)
        (real-part-polar z)))

(define (imag-part z)
    (if (rectangular? z)
        (imag-part-rect z)
        (imag-part-polar z)))

(define (make-from-real-imag a b)
    (set-tag 'rect (cons a b)))

;;; END: 2.29


;;; BEGIN: 2.30

; Base functions
(define (set-tag tag object) (cons tag object))
(define (get-tag tagged-object) (car tagged-object))
(define (get-contents tagged-object) (cdr tagged-object))
(define (rectangular? tagged-object) (eq? 'rect (get-tag tagged-object)))
(define (polar? tagged-object) (eq? 'polar (get-tag tagged-object)))

; Specific functions (real and imag)
(define (real-part-rect z) (car (get-contents z)))
(define (imag-part-rect z) (cdr (get-contents z)))
(define (real-part-polar z) (* (car (get-contents z)) (cos (cdr (get-contents z)))))
(define (imag-part-polar z) (* (car (get-contents z)) (sin (cdr (get-contents z)))))

; General functions (real and imag)
(define (real-part z)
    (if (rectangular? z)
        (real-part-rect z)
        (real-part-polar z)))

(define (imag-part z)
    (if (rectangular? z)
        (imag-part-rect z)
        (imag-part-polar z)))

; Making rect form complex number
(define (make-from-real-imag a b)
    (set-tag 'rect (cons a b)))


; Specific functions (magnitude and angle)
(define (magnitude-rect z)
      (sqrt (+ (* (real-part-rect z) (real-part-rect z)) (* (imag-part-rect z) (imag-part-rect z)))))

(define (magnitude-polar z) (car (get-contents z)))
(define (angle-rect z)
    (atan (imag-part-rect z) (real-part-rect z)))
(define (angle-polar z) (cdr (get-contents z)))

; General functions (magnitude and angle)
(define (magnitude z)
    (if (rectangular? z)
        (magnitude-rect z)
        (magnitude-polar z)))

(define (angle z)
    (if (rectangular? z)
        (angle-rect z)
        (angle-polar z)))

; Making polar form complex number
(define (make-from-mag-ang a b)
    (set-tag 'polar (cons a b)))


;(define test (make-from-real-imag 1 1))
;(real-part test)
;(imag-part test)
;(magnitude test)
;(angle test)

;;; END: 2.30


;;; BEGIN: 2.31

; Base functions
(define (set-tag tag object) (cons tag object))
(define (get-tag tagged-object) (car tagged-object))
(define (get-contents tagged-object) (cdr tagged-object))
(define (rectangular? tagged-object) (eq? 'rect (get-tag tagged-object)))
(define (polar? tagged-object) (eq? 'polar (get-tag tagged-object)))

; Specific functions (real and imag)
(define (real-part-rect z) (car (get-contents z)))
(define (imag-part-rect z) (cdr (get-contents z)))
(define (real-part-polar z) (* (car (get-contents z)) (cos (cdr (get-contents z)))))
(define (imag-part-polar z) (* (car (get-contents z)) (sin (cdr (get-contents z)))))

; General functions (real and imag)
(define (real-part z)
    (if (rectangular? z)
        (real-part-rect z)
        (real-part-polar z)))

(define (imag-part z)
    (if (rectangular? z)
        (imag-part-rect z)
        (imag-part-polar z)))

; Making rect form complex number
(define (make-from-real-imag a b)
    (set-tag 'rect (cons a b)))


; Specific functions (magnitude and angle)
(define (magnitude-rect z)
      (sqrt (+ (* (real-part-rect z) (real-part-rect z)) (* (imag-part-rect z) (imag-part-rect z)))))

(define (magnitude-polar z) (car (get-contents z)))
(define (angle-rect z)
    (atan (imag-part-rect z) (real-part-rect z)))
(define (angle-polar z) (cdr (get-contents z)))

; General functions (magnitude and angle)
(define (magnitude z)
    (if (rectangular? z)
        (magnitude-rect z)
        (magnitude-polar z)))

(define (angle z)
    (if (rectangular? z)
        (angle-rect z)
        (angle-polar z)))

; Making polar form complex number
(define (make-from-mag-ang a b)
    (set-tag 'polar (cons a b)))



(define (add-complex z1 z2)
      (make-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
      (make-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
      (make-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
      (make-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))


;(define a (/ (sqrt 2) 2))
;(define z (make-from-real-imag a a))
;(get-tag z)
;(get-contents z)
;(magnitude z)

;(define w (mul-complex z z))
;(get-tag w)

;;; END: 2.31


;;; BEGIN: 2.32
; You would have to keep adding conditions for each tag and it would not be pratical to do that for many functions
;;; END: 2.32


;;; BEGIN: 2.33

;;; set-up: table
(define table '())
(define (table-set name tag proc)
      (set! table (cons (list name tag proc) table)))
(define (table-get name tag)
      (define (inner my-table)
                (if (and (eq? name (caar my-table))
                                          (eq? tag (cadar my-table)))
                              (caddar my-table)
                                          (inner (cdr my-table))))
          (inner table))
 
;;; set-up: tag helper functions go here
(define (set-tag tag object) (cons tag object))
(define (get-tag tagged-object) (car tagged-object))
(define (get-contents tagged-object) (cdr tagged-object))

;;; definitions of packages
(define (install-rectangular-package)
      ;; constructor and accessor code
    (define (make-from-real-imag a b) (cons a b))
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (magnitude z) (sqrt (+ (* (real-part z) (real-part z)) (* (imag-part z) (imag-part z)))))
    (define (angle z) (atan (real-part z) (imag-part z)))

      ;; registration code
    (table-set 'make-from-real-imag 'rect (lambda (a b) (cons 'rect (make-from-real-imag a b))))
    (table-set 'real-part 'rect real-part)
    (table-set 'imag-part 'rect imag-part)
    (table-set 'magnitude 'rect magnitude)
    (table-set 'angle 'rect angle)
           
              'done)


(define (install-polar-package)
      ;; constructor and accessor code
    (define (make-from-mag-ang a b) (cons a b))
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (real-part z) (* (car z) (cos (cdr z))))
    (define (imag-part z) (* (car z) (sin (cdr z))))

       ;; registration code
    (table-set 'make-from-mag-ang 'polar (lambda (a b) (cons 'polar (make-from-mag-ang a b))))
    (table-set 'real-part 'polar real-part)
    (table-set 'imag-part 'polar imag-part)
    (table-set 'magnitude 'polar magnitude)
    (table-set 'angle 'polar angle)
           
              'done)

;;; apply-generic and the generic accessors and constructors go here.
(define (apply-generic op . args)
    ; gets type of arg (get-tag (car args)) and applies right function from table
    ; have to take (car args) first because args will be a list in itself with the inputted args 
    ;(map (lambda (arg) (apply (table-get op (get-tag (car args))) (list (get-contents arg)))) args))
    (apply (table-get op (get-tag (car args))) (map get-contents args)))


(define (real-part z)
    (apply-generic 'real-part z))

(define (imag-part z)
    (apply-generic 'imag-part z))

(define (magnitude z)
    (apply-generic 'magnitude z))

(define (angle z)
    (apply-generic 'angle z))


(define (make-from-real-imag a b)
    ((table-get 'make-from-real-imag 'rect) a b))

(define (make-from-mag-ang r theta)
    ((table-get 'make-from-mag-ang 'polar) r theta))


;;; install packages:
(install-rectangular-package)
(install-polar-package)


;;; copy your level-two procedures here.
(define (add-complex z1 z2)
      (make-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
      (make-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
      (make-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
      (make-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))


;;; And now test!

(define z (make-from-mag-ang 2 0))
(define w (make-from-mag-ang 3 (/ 3.14 2)))

(define i (make-from-real-imag 0 1))
(mul-complex i i)
(add-complex i i)

(define m1 (make-from-real-imag -1 0))
(mul-complex m1 m1)


;;; END: 2.33


;;; BEGIN: 2.34

;;; set-up: table
(define table '())
(define (table-set name tag proc)
      (set! table (cons (list name tag proc) table)))
(define (table-get name tag)
      (define (inner my-table)
                (if (and (equal? name (caar my-table))
                                          (equal? tag (cadar my-table)))
                              (caddar my-table)
                                          (inner (cdr my-table))))
          (inner table))
 
;;; set-up: tag helper functions go here
(define (set-tag tag object) (cons tag object))
(define (get-tag tagged-object) (car tagged-object))
(define (get-contents tagged-object) (cdr tagged-object))


;;; LISP NUMBER PACKAGE

(define (install-lisp-number-package)
    ;; constructer and accessor code
    (define (make-lisp-num n) n)
    (define (add a b) (+ a b))
    (define (sub a b) (- a b))
    (define (mul a b) (* a b))
    (define (div a b) (/ a b))
    (define (eq? a b) (= a b))
    
    ;; registration code
    (table-set 'make-lisp-num '(lisp-number) (lambda (n) (set-tag 'lisp-number (make-lisp-num n))))
    (table-set 'add '(lisp-number lisp-number) (lambda (a b) (set-tag 'lisp-number (add a b))))
    (table-set 'sub '(lisp-number lisp-number) (lambda (a b) (set-tag 'lisp-number (sub a b))))
    (table-set 'mul '(lisp-number lisp-number) (lambda (a b) (set-tag 'lisp-number (mul a b))))
    (table-set 'div '(lisp-number lisp-number) (lambda (a b) (set-tag 'lisp-number (div a b))))
    
        'done)



;;; RATIONAL NUMBERS PACKAGE

(define (install-rational-package)
    ;; constructer and accessor code
    (define (numer rat) (car rat))
    (define (denom rat) (cdr rat))
    (define (make-rat x y)
        (cond ((and (< x 0) (> y 0)) (cons (/ (abs x) (gcd x y)) (* (/ y (gcd x y)) -1)))
              ((and (> x 0) (< y 0)) (cons (/ x (gcd x y)) (* (/ y (gcd x y)))))
          (else (cons (/ (abs x) (gcd x y)) (/ (abs y) (gcd x y))))))

    ;; level 2 functions 
    (define (add-rat rat1 rat2) (make-rat (+ (* (numer rat1) (denom rat2)) (* (numer rat2) (denom rat1))) (* (denom rat1) (denom rat2))))
    (define (sub-rat rat1 rat2) (make-rat (- (* (numer rat1) (denom rat2)) (* (numer rat2) (denom rat1))) (* (denom rat1) (denom rat2))))
    (define (mul-rat rat1 rat2) (make-rat (* (numer rat1) (numer rat2)) (* (denom rat1) (denom rat2))))
    (define (div-rat rat1 rat2) (make-rat (* (numer rat1) (denom rat2)) (* (denom rat1) (numer rat2))))

    ;; registration code 
    (table-set 'make-rat '(rational) (lambda (a b) (set-tag 'rational (make-rat a b))))
    (table-set 'add '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (add-rat rat1 rat2))))
    (table-set 'sub '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (sub-rat rat1 rat2))))
    (table-set 'mul '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (mul-rat rat1 rat2))))
    (table-set 'div '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (div-rat rat1 rat2))))

        'done)



;;; COMPLEX NUMBERS PACKAGE

(define (install-complex-package)
    ;; RECTANGULAR SUBPACKAGE
     (define (install-rectangular-package)
          ;; constructor and accessor code
        (define (make-from-real-imag a b) (cons a b))
        (define (real-part z) (car z))
        (define (imag-part z) (cdr z)) 
        (define (magnitude z) (sqrt (+ (* (real-part z) (real-part z)) (* (imag-part z) (imag-part z)))))
        (define (angle z) (atan (imag-part z) (real-part z)))

          ;; registration code
        (table-set 'make-from-real-imag '(rect) (lambda (a b) (set-tag 'rect (make-from-real-imag a b))))
        (table-set 'real-part '(rect) real-part)
        (table-set 'imag-part '(rect) imag-part)
        (table-set 'magnitude '(rect) magnitude)
        (table-set 'angle '(rect) angle)
               
                  'done)

    ;; POLAR SUBPACKAGE
    (define (install-polar-package)
          ;; constructor and accessor code 
        (define (make-from-mag-ang a b) (cons a b))
        (define (magnitude z) (car z))
        (define (angle z) (cdr z)) 
        (define (real-part z) (* (magnitude z) (cos (angle z))))
        (define (imag-part z) (* (magnitude z) (sin (angle z))))

           ;; registration code 
        (table-set 'make-from-mag-ang '(polar) (lambda (a b) (set-tag 'polar (make-from-mag-ang a b))))
        (table-set 'real-part '(polar) real-part)
        (table-set 'imag-part '(polar) imag-part)
        (table-set 'magnitude '(polar) magnitude)
        (table-set 'angle '(polar) angle)
           
              'done)
    
    ;; Adding packages for use in rest of complex package
    (install-rectangular-package)
    (install-polar-package)

    ;; constructor code
    (define (make-complex-from-real-imag a b) (apply (table-get 'make-from-real-imag '(rect)) (list a b)))
    (define (make-complex-from-mag-ang r theta) (apply (table-get 'make-from-mag-ang '(polar)) (list r theta)))
    
    ;; defining for use in the level two procedures (only for complex here) (has to look up '(rect) and '(polar) so finding tag and putting it in a list for lookup)
    (define (apply-generic op . args) (apply (table-get op (list (get-tag (car args)))) (map get-contents args)))
    (define (real-part z) (apply-generic 'real-part z))
    (define (imag-part z) (apply-generic 'imag-part z))
    (define (magnitude z) (apply-generic 'magnitude z))
    (define (angle z) (apply-generic 'angle z))

    ;; level two procedures
    (define (add z1 z2) (make-complex-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2))))
    (define (sub z1 z2) (make-complex-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))
    (define (mul z1 z2) (make-complex-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2))))
    (define (div z1 z2) (make-complex-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))

    ;; registration code 
    (table-set 'make-complex-from-real-imag '(complex) (lambda (a b) (set-tag 'complex (make-complex-from-real-imag a b))))
    (table-set 'make-complex-from-mag-ang '(complex) (lambda (r theta) (set-tag 'complex (make-complex-from-mag-ang r theta))))
    (table-set 'add '(complex complex) (lambda (z1 z2) (set-tag 'complex (add z1 z2))))
    (table-set 'sub '(complex complex) (lambda (z1 z2) (set-tag 'complex (sub z1 z2))))
    (table-set 'mul '(complex complex) (lambda (z1 z2) (set-tag 'complex (mul z1 z2))))
    (table-set 'div '(complex complex) (lambda (z1 z2) (set-tag 'complex (div z1 z2))))


        'done)



;;; INSTALLING PACKAGES
(install-lisp-number-package)
(install-rational-package)
(install-complex-package)

;;; APPLY GENERIC 
(define (apply-generic op . args)
    (apply (table-get op (map get-tag args)) (map get-contents args)))

;;; OUTSIDE CONSTRUCTERS (FOR lisp-number, rational, complex rect and complex polar)
(define (make-lisp-num a) ((table-get 'make-lisp-num '(lisp-number)) a))
(define (make-rat numer denom) (apply (table-get 'make-rat '(rational)) (list numer denom)))
(define (make-complex-from-real-imag a b) (apply (table-get 'make-complex-from-real-imag '(complex)) (list a b)))
(define (make-complex-from-mag-ang r theta) (apply (table-get 'make-complex-from-mag-ang '(complex)) (list r theta)))

;;; displaying table (for debugging purposes)
(display table)

;; Setting tolerance to 0.00001
(define tolerance 0.00001)

;; level 2 fucntions with apply-generic
(define (add n m) (apply-generic 'add n m))
(define (subtract n m) (apply-generic 'sub n m))
(define (multiply n m) (apply-generic 'mul n m))
(define (divide n m) (apply-generic 'div n m))

;;; TEST CODE (with all types of numbers)
;(define test-lisp-num (make-lisp-num 2))
;(apply-generic 'add test-lisp-num test-lisp-num)
;(apply-generic 'sub test-lisp-num test-lisp-num)
;(apply-generic 'mul test-lisp-num test-lisp-num)
;(apply-generic 'div test-lisp-num test-lisp-num)

;(define test-rat (make-rat 1 3))
;(apply-generic 'add test-rat test-rat)
;(apply-generic 'sub test-rat test-rat)
;(apply-generic 'mul test-rat test-rat)
;(apply-generic 'div test-rat test-rat)

;(define test-complex-real-imag (make-complex-from-real-imag 1 0))
;(apply-generic 'add test-complex-real-imag test-complex-real-imag) 
;(apply-generic 'sub test-complex-real-imag test-complex-real-imag) 
;(apply-generic 'mul test-complex-real-imag test-complex-real-imag) 
;(apply-generic 'div test-complex-real-imag test-complex-real-imag) 

;(define test-complex-mag-ang (make-complex-from-mag-ang 1 (/ 3.1415296 4)))
;(apply-generic 'add test-complex-mag-ang test-complex-mag-ang)
;(apply-generic 'sub test-complex-mag-ang test-complex-mag-ang)
;(apply-generic 'mul test-complex-mag-ang test-complex-mag-ang)
;(apply-generic 'div test-complex-mag-ang test-complex-mag-ang)


;;; test code
(define a (make-lisp-num 14))
(multiply a a) ;returns '(lisp-number 196)
(define z (make-complex-from-real-imag 0.707 0.707)) ; 0.707 = 1/sqrt{2}

(multiply z z) ;returns approx '(complex polar 1 . 1.57)


;;; END: 2.34


;;; BEGIN: 2.35

;;; set-up: table
(define table '())
(define (table-set name tag proc)
      (set! table (cons (list name tag proc) table)))
(define (table-get name tag)
      (define (inner my-table)
                (if (and (equal? name (caar my-table))
                                          (equal? tag (cadar my-table)))
                              (caddar my-table)
                                          (inner (cdr my-table))))
          (inner table))
 
;;; set-up: tag helper functions go here
(define (set-tag tag object) (cons tag object))
(define (get-tag tagged-object) (car tagged-object))
(define (get-contents tagged-object) (cdr tagged-object))

;;; DEFINING TOLERANCE as 0.00001
(define tolerance 0.00001)


;;; LISP NUMBER PACKAGE

(define (install-lisp-number-package)
    ;; constructer and accessor code
    (define (make-lisp-num n) n)
    (define (add a b) (+ a b))
    (define (sub a b) (- a b))
    (define (mul a b) (* a b))
    (define (div a b) (/ a b))
    (define (eq-num? a b) (< (- a b) tolerance))
    
    ;; registration code
    (table-set 'make-lisp-num '(lisp-number) (lambda (n) (set-tag 'lisp-number (make-lisp-num n))))
    (table-set 'add '(lisp-number lisp-number) (lambda (a b) (set-tag 'lisp-number (add a b))))
    (table-set 'sub '(lisp-number lisp-number) (lambda (a b) (set-tag 'lisp-number (sub a b))))
    (table-set 'mul '(lisp-number lisp-number) (lambda (a b) (set-tag 'lisp-number (mul a b))))
    (table-set 'div '(lisp-number lisp-number) (lambda (a b) (set-tag 'lisp-number (div a b))))
    (table-set 'eq-num? '(lisp-number lisp-number) (lambda (a b) (eq-num? a b)))
    
        'done)



;;; RATIONAL NUMBERS PACKAGE

(define (install-rational-package)
    ;; constructer and accessor code
    (define (numer rat) (car rat))
    (define (denom rat) (cdr rat))
    (define (make-rat x y)
        (cond ((and (< x 0) (> y 0)) (cons (/ (abs x) (gcd x y)) (* (/ y (gcd x y)) -1)))
              ((and (> x 0) (< y 0)) (cons (/ x (gcd x y)) (* (/ y (gcd x y)))))
          (else (cons (/ (abs x) (gcd x y)) (/ (abs y) (gcd x y))))))

    ;; level 2 functions 
    (define (add-rat rat1 rat2) (make-rat (+ (* (numer rat1) (denom rat2)) (* (numer rat2) (denom rat1))) (* (denom rat1) (denom rat2))))
    (define (sub-rat rat1 rat2) (make-rat (- (* (numer rat1) (denom rat2)) (* (numer rat2) (denom rat1))) (* (denom rat1) (denom rat2))))
    (define (mul-rat rat1 rat2) (make-rat (* (numer rat1) (numer rat2)) (* (denom rat1) (denom rat2))))
    (define (div-rat rat1 rat2) (make-rat (* (numer rat1) (denom rat2)) (* (denom rat1) (numer rat2))))
    (define (eq-num? rat1 rat2) (< (- (/ (numer rat1) (denom rat1)) (/ (numer rat2) (denom rat2))) tolerance))

    ;; registration code 
    (table-set 'make-rat '(rational) (lambda (a b) (set-tag 'rational (make-rat a b))))
    (table-set 'add '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (add-rat rat1 rat2))))
    (table-set 'sub '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (sub-rat rat1 rat2))))
    (table-set 'mul '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (mul-rat rat1 rat2))))
    (table-set 'div '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (div-rat rat1 rat2))))
    (table-set 'eq-num? '(rational rational) (lambda (rat1 rat2) (eq-num? rat1 rat2)))

        'done)



;;; COMPLEX NUMBERS PACKAGE

(define (install-complex-package)
    ;; RECTANGULAR SUBPACKAGE
     (define (install-rectangular-package)
          ;; constructor and accessor code
        (define (make-from-real-imag a b) (cons a b))
        (define (real-part z) (car z))
        (define (imag-part z) (cdr z)) 
        (define (magnitude z) (sqrt (+ (* (real-part z) (real-part z)) (* (imag-part z) (imag-part z)))))
        (define (angle z) (atan (imag-part z) (real-part z)))

          ;; registration code
        (table-set 'make-from-real-imag '(rect) (lambda (a b) (set-tag 'rect (make-from-real-imag a b))))
        (table-set 'real-part '(rect) real-part)
        (table-set 'imag-part '(rect) imag-part)
        (table-set 'magnitude '(rect) magnitude)
        (table-set 'angle '(rect) angle)
               
                  'done)

    ;; POLAR SUBPACKAGE
    (define (install-polar-package)
          ;; constructor and accessor code 
        (define (make-from-mag-ang a b) (cons a b))
        (define (magnitude z) (car z))
        (define (angle z) (cdr z)) 
        (define (real-part z) (* (magnitude z) (cos (angle z))))
        (define (imag-part z) (* (magnitude z) (sin (angle z))))

           ;; registration code 
        (table-set 'make-from-mag-ang '(polar) (lambda (a b) (set-tag 'polar (make-from-mag-ang a b))))
        (table-set 'real-part '(polar) real-part)
        (table-set 'imag-part '(polar) imag-part)
        (table-set 'magnitude '(polar) magnitude)
        (table-set 'angle '(polar) angle)
           
              'done)
    
    ;; Adding packages for use in rest of complex package
    (install-rectangular-package)
    (install-polar-package)

    ;; constructor code
    (define (make-complex-from-real-imag a b) (apply (table-get 'make-from-real-imag '(rect)) (list a b)))
    (define (make-complex-from-mag-ang r theta) (apply (table-get 'make-from-mag-ang '(polar)) (list r theta)))
    
    ;; defining for use in the level two procedures (only for complex here)
    (define (apply-generic op . args) (apply (table-get op (list (get-tag (car args)))) (map get-contents args)))
    (define (real-part z) (apply-generic 'real-part z))
    (define (imag-part z) (apply-generic 'imag-part z))
    (define (magnitude z) (apply-generic 'magnitude z))
    (define (angle z) (apply-generic 'angle z))

    ;; level two procedures
    (define (add z1 z2) (make-complex-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2))))
    (define (sub z1 z2) (make-complex-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))
    (define (mul z1 z2) (make-complex-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2))))
    (define (div z1 z2) (make-complex-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))
    (define (eq-num? z1 z2) (and (< (- (real-part z1) (real-part z2)) tolerance) (< (- (real-part z1) (real-part z2)) tolerance)))

    ;; registration code 
    (table-set 'make-complex-from-real-imag '(complex) (lambda (a b) (set-tag 'complex (make-complex-from-real-imag a b))))
    (table-set 'make-complex-from-mag-ang '(complex) (lambda (r theta) (set-tag 'complex (make-complex-from-mag-ang r theta))))
    (table-set 'add '(complex complex) (lambda (z1 z2) (set-tag 'complex (add z1 z2))))
    (table-set 'sub '(complex complex) (lambda (z1 z2) (set-tag 'complex (sub z1 z2))))
    (table-set 'mul '(complex complex) (lambda (z1 z2) (set-tag 'complex (mul z1 z2))))
    (table-set 'div '(complex complex) (lambda (z1 z2) (set-tag 'complex (div z1 z2))))
    (table-set 'eq-num? '(complex complex) (lambda (z1 z2) (eq-num? z1 z2)))


        'done)



;;; INSTALLING PACKAGES
(install-lisp-number-package)
(install-rational-package)
(install-complex-package)

;;; APPLY GENERIC 
(define (apply-generic op . args)
    (apply (table-get op (map get-tag args)) (map get-contents args)))

;;; OUTSIDE CONSTRUCTERS (FOR lisp-number, rational, complex rect and complex polar)
(define (make-lisp-num a) ((table-get 'make-lisp-num '(lisp-number)) a))
(define (make-rat numer denom) (apply (table-get 'make-rat '(rational)) (list numer denom)))
(define (make-complex-from-real-imag a b) (apply (table-get 'make-complex-from-real-imag '(complex)) (list a b)))
(define (make-complex-from-mag-ang r theta) (apply (table-get 'make-complex-from-mag-ang '(complex)) (list r theta)))


;;; displaying table (for debugging purposes)
(display table)

;; level 2 fucntions with apply-generic
(define (add n m) (apply-generic 'add n m))
(define (subtract n m) (apply-generic 'sub n m))
(define (multiply n m) (apply-generic 'mul n m))
(define (divide n m) (apply-generic 'div n m))
(define (eq-num? n m) (apply-generic 'eq-num? n m))

;;; TEST CODE (with all types of numbers)
;(define test-lisp-num (make-lisp-num 2))
;(apply-generic 'add test-lisp-num test-lisp-num)
;(apply-generic 'sub test-lisp-num test-lisp-num)
;(apply-generic 'mul test-lisp-num test-lisp-num)
;(apply-generic 'div test-lisp-num test-lisp-num)

;(define test-rat (make-rat 1 3))
;(apply-generic 'add test-rat test-rat)
;(apply-generic 'sub test-rat test-rat)
;(apply-generic 'mul test-rat test-rat)
;(apply-generic 'div test-rat test-rat)

;(define test-complex-real-imag (make-complex-from-real-imag 1 0))
;(apply-generic 'add test-complex-real-imag test-complex-real-imag) 
;(apply-generic 'sub test-complex-real-imag test-complex-real-imag) 
;(apply-generic 'mul test-complex-real-imag test-complex-real-imag) 
;(apply-generic 'div test-complex-real-imag test-complex-real-imag) 

;(define test-complex-mag-ang (make-complex-from-mag-ang 1 (/ 3.1415296 4)))
;(apply-generic 'add test-complex-mag-ang test-complex-mag-ang)
;(apply-generic 'sub test-complex-mag-ang test-complex-mag-ang)
;(apply-generic 'mul test-complex-mag-ang test-complex-mag-ang)
;(apply-generic 'div test-complex-mag-ang test-complex-mag-ang)


;;; test code
;(define a (make-lisp-num 14))
;(define b (make-rat 5 6))
;(define c (make-rat 59 60))
;(eq-num? b c)

;;; END: 2.35


;;; BEGIN: 2.36

; The error means that we have not registered magnitude and angle functions for the '(complex tag), only for '(rect) and '(polar)

;;; set-up: table
(define table '())
(define (table-set name tag proc)
      (set! table (cons (list name tag proc) table)))
(define (table-get name tag)
      (define (inner my-table)
                (if (and (equal? name (caar my-table))
                                          (equal? tag (cadar my-table)))
                              (caddar my-table)
                                          (inner (cdr my-table))))
          (inner table))
 
;;; set-up: tag helper functions go here
(define (set-tag tag object) (cons tag object))
(define (get-tag tagged-object) (car tagged-object))
(define (get-contents tagged-object) (cdr tagged-object))

;;; DEFINING TOLERANCE as 0.00001
(define tolerance 0.00001)


;;; LISP NUMBER PACKAGE

(define (install-lisp-number-package)
    ;; constructer and accessor code
    (define (make-lisp-num n) n)
    (define (add a b) (+ a b))
    (define (sub a b) (- a b))
    (define (mul a b) (* a b))
    (define (div a b) (/ a b))
    (define (eq-num? a b) (< (- a b) tolerance))
    
    ;; registration code
    (table-set 'make-lisp-num '(lisp-number) (lambda (n) (set-tag 'lisp-number (make-lisp-num n))))
    (table-set 'add '(lisp-number lisp-number) (lambda (a b) (set-tag 'lisp-number (add a b))))
    (table-set 'sub '(lisp-number lisp-number) (lambda (a b) (set-tag 'lisp-number (sub a b))))
    (table-set 'mul '(lisp-number lisp-number) (lambda (a b) (set-tag 'lisp-number (mul a b))))
    (table-set 'div '(lisp-number lisp-number) (lambda (a b) (set-tag 'lisp-number (div a b))))
    (table-set 'eq-num? '(lisp-number lisp-number) (lambda (a b) (eq-num? a b)))
    
        'done)



;;; RATIONAL NUMBERS PACKAGE

(define (install-rational-package)
    ;; constructer and accessor code
    (define (numer rat) (car rat))
    (define (denom rat) (cdr rat))
    (define (make-rat x y)
        (cond ((and (< x 0) (> y 0)) (cons (/ (abs x) (gcd x y)) (* (/ y (gcd x y)) -1)))
              ((and (> x 0) (< y 0)) (cons (/ x (gcd x y)) (* (/ y (gcd x y)))))
          (else (cons (/ (abs x) (gcd x y)) (/ (abs y) (gcd x y))))))

    ;; level 2 functions 
    (define (add-rat rat1 rat2) (make-rat (+ (* (numer rat1) (denom rat2)) (* (numer rat2) (denom rat1))) (* (denom rat1) (denom rat2))))
    (define (sub-rat rat1 rat2) (make-rat (- (* (numer rat1) (denom rat2)) (* (numer rat2) (denom rat1))) (* (denom rat1) (denom rat2))))
    (define (mul-rat rat1 rat2) (make-rat (* (numer rat1) (numer rat2)) (* (denom rat1) (denom rat2))))
    (define (div-rat rat1 rat2) (make-rat (* (numer rat1) (denom rat2)) (* (denom rat1) (numer rat2))))
    (define (eq-num? rat1 rat2) (< (- (/ (numer rat1) (denom rat1)) (/ (numer rat2) (denom rat2))) tolerance))

    ;; registration code 
    (table-set 'make-rat '(rational) (lambda (a b) (set-tag 'rational (make-rat a b))))
    (table-set 'add '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (add-rat rat1 rat2))))
    (table-set 'sub '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (sub-rat rat1 rat2))))
    (table-set 'mul '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (mul-rat rat1 rat2))))
    (table-set 'div '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (div-rat rat1 rat2))))
    (table-set 'eq-num? '(rational rational) (lambda (rat1 rat2) (eq-num? rat1 rat2)))

        'done)



;;; COMPLEX NUMBERS PACKAGE

(define (install-complex-package)
    ;; RECTANGULAR SUBPACKAGE
     (define (install-rectangular-package)
          ;; constructor and accessor code
        (define (make-from-real-imag a b) (cons a b))
        (define (real-part z) (car z))
        (define (imag-part z) (cdr z)) 
        (define (magnitude z) (sqrt (+ (* (real-part z) (real-part z)) (* (imag-part z) (imag-part z)))))
        (define (angle z) (atan (imag-part z) (real-part z)))

          ;; registration code
        (table-set 'make-from-real-imag '(rect) (lambda (a b) (set-tag 'rect (make-from-real-imag a b))))
        (table-set 'real-part '(rect) real-part)
        (table-set 'imag-part '(rect) imag-part)
        (table-set 'magnitude '(rect) magnitude)
        (table-set 'angle '(rect) angle)
               
                  'done)

    ;; POLAR SUBPACKAGE
    (define (install-polar-package)
          ;; constructor and accessor code 
        (define (make-from-mag-ang a b) (cons a b))
        (define (magnitude z) (car z))
        (define (angle z) (cdr z)) 
        (define (real-part z) (* (magnitude z) (cos (angle z))))
        (define (imag-part z) (* (magnitude z) (sin (angle z))))

           ;; registration code 
        (table-set 'make-from-mag-ang '(polar) (lambda (a b) (set-tag 'polar (make-from-mag-ang a b))))
        (table-set 'real-part '(polar) real-part)
        (table-set 'imag-part '(polar) imag-part)
        (table-set 'magnitude '(polar) magnitude)
        (table-set 'angle '(polar) angle)
           
              'done)
    
    ;; Adding packages for use in rest of complex package
    (install-rectangular-package)
    (install-polar-package)

    ;; constructor code
    (define (make-complex-from-real-imag a b) (apply (table-get 'make-from-real-imag '(rect)) (list a b)))
    (define (make-complex-from-mag-ang r theta) (apply (table-get 'make-from-mag-ang '(polar)) (list r theta)))
    
    ;; defining for use in the level two procedures (only for complex here)
    (define (apply-generic op . args) (apply (table-get op (list (get-tag (car args)))) (map get-contents args)))
    (define (real-part z) (apply-generic 'real-part z))
    (define (imag-part z) (apply-generic 'imag-part z))
    (define (magnitude z) (apply-generic 'magnitude z))
    (define (angle z) (apply-generic 'angle z))

    ;; level two procedures
    (define (add z1 z2) (make-complex-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2))))
    (define (sub z1 z2) (make-complex-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))
    (define (mul z1 z2) (make-complex-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2))))
    (define (div z1 z2) (make-complex-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))
    (define (eq-num? z1 z2) (and (< (- (real-part z1) (real-part z2)) tolerance) (< (- (real-part z1) (real-part z2)) tolerance)))

    ;; registration code 
    (table-set 'make-complex-from-real-imag '(complex) (lambda (a b) (set-tag 'complex (make-complex-from-real-imag a b))))
    (table-set 'make-complex-from-mag-ang '(complex) (lambda (r theta) (set-tag 'complex (make-complex-from-mag-ang r theta))))
    (table-set 'magnitude '(complex) magnitude)
    (table-set 'angle '(complex) angle)
    (table-set 'add '(complex complex) (lambda (z1 z2) (set-tag 'complex (add z1 z2))))
    (table-set 'sub '(complex complex) (lambda (z1 z2) (set-tag 'complex (sub z1 z2))))
    (table-set 'mul '(complex complex) (lambda (z1 z2) (set-tag 'complex (mul z1 z2))))
    (table-set 'div '(complex complex) (lambda (z1 z2) (set-tag 'complex (div z1 z2))))
    (table-set 'eq-num? '(complex complex) (lambda (z1 z2) (eq-num? z1 z2)))


        'done)



;;; INSTALLING PACKAGES
(install-lisp-number-package)
(install-rational-package)
(install-complex-package)

;;; APPLY GENERIC 
(define (apply-generic op . args)
    (apply (table-get op (map get-tag args)) (map get-contents args)))

;;; OUTSIDE CONSTRUCTERS (FOR lisp-number, rational, complex rect and complex polar)
(define (make-lisp-num a) ((table-get 'make-lisp-num '(lisp-number)) a))
(define (make-rat numer denom) (apply (table-get 'make-rat '(rational)) (list numer denom)))
(define (make-complex-from-real-imag a b) (apply (table-get 'make-complex-from-real-imag '(complex)) (list a b)))
(define (make-complex-from-mag-ang r theta) (apply (table-get 'make-complex-from-mag-ang '(complex)) (list r theta)))


;;; displaying table (for debugging purposes)
(display table)

;; level 2 fucntions with apply-generic
(define (add n m) (apply-generic 'add n m))
(define (subtract n m) (apply-generic 'sub n m))
(define (multiply n m) (apply-generic 'mul n m))
(define (divide n m) (apply-generic 'div n m))
(define (eq-num? n m) (apply-generic 'eq-num? n m))

;;; test code
(define a (make-lisp-num 14))
(multiply a a) ;returns '(lisp-number 196)
(define z (make-complex-from-real-imag 0.707 0.707)) ; 0.707 = 1/sqrt{2}

(multiply z z) ;returns approx '(complex polar 1 . 1.57)
(apply-generic 'magnitude z)

;;; END: 2.36


;;; BEGIN: 2.37

(define type-tower (list 'integer 'rational 'real 'complex))

;;; set-up: table
(define table '())
(define (table-set name tag proc)
      (set! table (cons (list name tag proc) table)))
(define (table-get name tag)
      ; returns 'none if function isn't in table
      (define (inner my-table)
                (if (null? my-table)
                    'none
                    (if (and (equal? name (caar my-table))
                                              (equal? tag (cadar my-table)))
                                  (caddar my-table)
                                              (inner (cdr my-table))))
                )
          (inner table))
 
;;; set-up: tag helper functions go here
(define (set-tag tag object) (cons tag object))
(define (get-tag tagged-object) (car tagged-object))
(define (get-contents tagged-object) (cdr tagged-object))

;;; DEFINING TOLERANCE as 0.00001
(define tolerance 0.00001)


;;; INTEGER PACKAGE

(define (install-integer-package)
    (define (make-integer x) (set-tag 'integer x))
    (table-set 'make-integer '(integer) (lambda (x) (make-integer x)))
    (table-set 'eq-num? '(integer integer) (lambda (n m) (= n m)))
        'done)

;;; RATIONAL NUMBERS PACKAGE

(define (install-rational-package)
    ;; constructer and accessor code
    (define (numer rat) (car rat))
    (define (denom rat) (cdr rat))
    (define (make-rat x y)
        (cond ((and (< x 0) (> y 0)) (cons (/ (abs x) (gcd x y)) (* (/ y (gcd x y)) -1)))
              ((and (> x 0) (< y 0)) (cons (/ x (gcd x y)) (* (/ y (gcd x y)))))
          (else (cons (/ (abs x) (gcd x y)) (/ (abs y) (gcd x y))))))

    ;; level 2 functions 
    (define (add-rat rat1 rat2) (make-rat (+ (* (numer rat1) (denom rat2)) (* (numer rat2) (denom rat1))) (* (denom rat1) (denom rat2))))
    (define (sub-rat rat1 rat2) (make-rat (- (* (numer rat1) (denom rat2)) (* (numer rat2) (denom rat1))) (* (denom rat1) (denom rat2))))
    (define (mul-rat rat1 rat2) (make-rat (* (numer rat1) (numer rat2)) (* (denom rat1) (denom rat2))))
    (define (div-rat rat1 rat2) (make-rat (* (numer rat1) (denom rat2)) (* (denom rat1) (numer rat2))))
    (define (eq-num? rat1 rat2) (< (- (/ (numer rat1) (denom rat1)) (/ (numer rat2) (denom rat2))) tolerance))

    ;; registration code 
    (table-set 'make-rat '(rational) (lambda (a b) (set-tag 'rational (make-rat a b))))
    (table-set 'add '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (add-rat rat1 rat2))))
    (table-set 'sub '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (sub-rat rat1 rat2))))
    (table-set 'mul '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (mul-rat rat1 rat2))))
    (table-set 'div '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (div-rat rat1 rat2))))
    (table-set 'eq-num? '(rational rational) (lambda (rat1 rat2) (eq-num? rat1 rat2)))

        'done)


;;; REAL NUMBER PACKAGE

(define (install-real-num-package)
    (define (make-real-num x) (set-tag 'real-num x))
    (table-set 'make-real-num '(real-num) (lambda (n) (make-real-num n)))
    (table-set 'eq-num? '(real-num real-num) (lambda (n m) (< (- n m) tolerance)))
        'done)


;;; COMPLEX NUMBERS PACKAGE

(define (install-complex-package)
    ;; RECTANGULAR SUBPACKAGE
     (define (install-rectangular-package)
          ;; constructor and accessor code
        (define (make-from-real-imag a b) (cons a b))
        (define (real-part z) (car z))
        (define (imag-part z) (cdr z)) 
        (define (magnitude z) (sqrt (+ (* (real-part z) (real-part z)) (* (imag-part z) (imag-part z)))))
        (define (angle z) (atan (imag-part z) (real-part z)))

          ;; registration code
        (table-set 'make-from-real-imag '(rect) (lambda (a b) (set-tag 'rect (make-from-real-imag a b))))
        (table-set 'real-part '(rect) real-part)
        (table-set 'imag-part '(rect) imag-part)
        (table-set 'magnitude '(rect) magnitude)
        (table-set 'angle '(rect) angle)
               
                  'done)

    ;; POLAR SUBPACKAGE
    (define (install-polar-package)
          ;; constructor and accessor code 
        (define (make-from-mag-ang a b) (cons a b))
        (define (magnitude z) (car z))
        (define (angle z) (cdr z)) 
        (define (real-part z) (* (magnitude z) (cos (angle z))))
        (define (imag-part z) (* (magnitude z) (sin (angle z))))

           ;; registration code 
        (table-set 'make-from-mag-ang '(polar) (lambda (a b) (set-tag 'polar (make-from-mag-ang a b))))
        (table-set 'real-part '(polar) real-part)
        (table-set 'imag-part '(polar) imag-part)
        (table-set 'magnitude '(polar) magnitude)
        (table-set 'angle '(polar) angle)
           
              'done)
    
    ;; Adding packages for use in rest of complex package
    (install-rectangular-package)
    (install-polar-package)

    ;; constructor code
    (define (make-complex-from-real-imag a b) (apply (table-get 'make-from-real-imag '(rect)) (list a b)))
    (define (make-complex-from-mag-ang r theta) (apply (table-get 'make-from-mag-ang '(polar)) (list r theta)))
    
    ;; defining for use in the level two procedures (only for complex here)
    (define (apply-generic op . args) (apply (table-get op (list (get-tag (car args)))) (map get-contents args)))
    (define (real-part z) (apply-generic 'real-part z))
    (define (imag-part z) (apply-generic 'imag-part z))
    (define (magnitude z) (apply-generic 'magnitude z))
    (define (angle z) (apply-generic 'angle z))

    ;; level two procedures
    (define (add z1 z2) (make-complex-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2))))
    (define (sub z1 z2) (make-complex-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))
    (define (mul z1 z2) (make-complex-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2))))
    (define (div z1 z2) (make-complex-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))
    (define (eq-num? z1 z2) (and (< (- (real-part z1) (real-part z2)) tolerance) (< (- (real-part z1) (real-part z2)) tolerance)))

    ;; registration code 
    (table-set 'make-complex-from-real-imag '(complex) (lambda (a b) (set-tag 'complex (make-complex-from-real-imag a b))))
    (table-set 'make-complex-from-mag-ang '(complex) (lambda (r theta) (set-tag 'complex (make-complex-from-mag-ang r theta))))
    (table-set 'magnitude '(complex) magnitude)
    (table-set 'angle '(complex) angle)
    (table-set 'add '(complex complex) (lambda (z1 z2) (set-tag 'complex (add z1 z2))))
    (table-set 'sub '(complex complex) (lambda (z1 z2) (set-tag 'complex (sub z1 z2))))
    (table-set 'mul '(complex complex) (lambda (z1 z2) (set-tag 'complex (mul z1 z2))))
    (table-set 'div '(complex complex) (lambda (z1 z2) (set-tag 'complex (div z1 z2))))
    (table-set 'eq-num? '(complex complex) (lambda (z1 z2) (eq-num? z1 z2)))


        'done)

;;; INSTALLING PACKAGES
(install-integer-package)
(install-rational-package)
(install-real-num-package)
(install-complex-package)

;;; OUTSIDE CONSTRUCTERS (FOR lisp-number, rational, complex rect and complex polar)
(define (make-integer a) ((table-get 'make-integer '(integer)) a))
(define (make-rat numer denom) (apply (table-get 'make-rat '(rational)) (list numer denom)))
(define (make-real-num a) ((table-get 'make-real-num '(real-num)) a))
(define (make-complex-from-real-imag a b) (apply (table-get 'make-complex-from-real-imag '(complex)) (list a b)))
(define (make-complex-from-mag-ang r theta) (apply (table-get 'make-complex-from-mag-ang '(complex)) (list r theta)))

;;; RAISING FUNCTIONS
;; defining the functions
(define (raise-integer-to-rational x) (apply (table-get 'make-rat '(rational)) (list (cdr x) 1)))
(define (raise-rational-to-real x) ((table-get 'make-real-num '(real-num)) (/ (cadr x) (cddr x))))
(define (raise-real-to-complex x) (apply (table-get 'make-complex-from-real-imag '(complex)) (list (cdr x) 0)))
;; registering functions under 'raise
(table-set 'raise '(integer) (lambda (x) (raise-integer-to-rational x)))
(table-set 'raise '(rational) (lambda (x) (raise-rational-to-real x)))
(table-set 'raise '(real-num) (lambda (x) (raise-real-to-complex x)))

;;; APPLY GENERIC 

(define (apply-generic op . args)
    ; for telling which arg to raise
    (define (get-tower-tier num-obj)
        ; raises level until it gets to complex (tier 4 is integer, tier 3 is rational, tier 2 is real, tier 1 is complex)
        (if (equal? (get-tag num-obj) 'complex)
            1
            (+ 1 (get-tower-tier ((table-get 'raise (list (get-tag num-obj))) num-obj))))) ; raises to next level

    ; converts args to the same type and then applies the correct function from the table
    (cond ((not (equal? 'none (table-get op (map get-tag args))))
            (apply (table-get op (map get-tag args)) (map get-contents args)))
          ((> (get-tower-tier (car args)) (get-tower-tier (cadr args))) (apply-generic op ((table-get 'raise (list (get-tag (car args)))) (car args)) (cadr args)))
          ((< (get-tower-tier (car args)) (get-tower-tier (cadr args))) (apply-generic op (car args) ((table-get 'raise (list (get-tag (cadr args)))) (cadr args))))
          ((= (get-tower-tier (car args)) (get-tower-tier (cadr args))) (if (and (equal? (get-tag (car args)) 'complex) (equal? (get-tag (car args)) 'complex))
                                                                          (error "apply-generic does not work with current inputs")
                                                                          (apply-generic op ((table-get 'raise (list (get-tag (car args)))) (car args)) (cadr args))))
          (else (error "apply-generic does not work with current inputs")))
    )




;; level 2 fucntions with apply-generic
(define (add n m) (apply-generic 'add n m))
(define (subtract n m) (apply-generic 'sub n m))
(define (multiply n m) (apply-generic 'mul n m))
(define (divide n m) (apply-generic 'div n m))
(define (eq-num? n m) (apply-generic 'eq-num? n m))

;test code
;(define a (make-integer 6))
;(define b ((table-get 'raise '(integer)) a))
;(define c ((table-get 'raise '(rational)) b))
;(define d ((table-get 'raise '(real-num)) c))

;;; END: 2.37


;;; BEGIN: 2.38

(define type-tower (list 'integer 'rational 'real 'complex))

;;; set-up: table
(define table '())
(define (table-set name tag proc)
      (set! table (cons (list name tag proc) table)))
(define (table-get name tag)
      ; returns 'none if function isn't in table
      (define (inner my-table)
                (if (null? my-table)
                    'none
                    (if (and (equal? name (caar my-table))
                                              (equal? tag (cadar my-table)))
                                  (caddar my-table)
                                              (inner (cdr my-table))))
                )
          (inner table))
 
;;; set-up: tag helper functions go here
(define (set-tag tag object) (cons tag object))
(define (get-tag tagged-object) (car tagged-object))
(define (get-contents tagged-object) (cdr tagged-object))

;;; DEFINING TOLERANCE as 0.00001
(define tolerance 0.00001)


;;; INTEGER PACKAGE

(define (install-integer-package)
    (define (make-integer x) (set-tag 'integer x))
    (table-set 'make-integer '(integer) (lambda (x) (make-integer x)))
    (table-set 'eq-num? '(integer integer) (lambda (n m) (= n m)))
        'done)

;;; RATIONAL NUMBERS PACKAGE

(define (install-rational-package)
    ;; constructer and accessor code
    (define (numer rat) (car rat))
    (define (denom rat) (cdr rat))
    (define (make-rat x y)
        (cond ((and (< x 0) (> y 0)) (cons (/ (abs x) (gcd x y)) (* (/ y (gcd x y)) -1)))
              ((and (> x 0) (< y 0)) (cons (/ x (gcd x y)) (* (/ y (gcd x y)))))
          (else (cons (/ (abs x) (gcd x y)) (/ (abs y) (gcd x y))))))

    ;; level 2 functions 
    (define (add-rat rat1 rat2) (make-rat (+ (* (numer rat1) (denom rat2)) (* (numer rat2) (denom rat1))) (* (denom rat1) (denom rat2))))
    (define (sub-rat rat1 rat2) (make-rat (- (* (numer rat1) (denom rat2)) (* (numer rat2) (denom rat1))) (* (denom rat1) (denom rat2))))
    (define (mul-rat rat1 rat2) (make-rat (* (numer rat1) (numer rat2)) (* (denom rat1) (denom rat2))))
    (define (div-rat rat1 rat2) (make-rat (* (numer rat1) (denom rat2)) (* (denom rat1) (numer rat2))))
    (define (eq-num? rat1 rat2) (< (- (/ (numer rat1) (denom rat1)) (/ (numer rat2) (denom rat2))) tolerance))

    ;; registration code 
    (table-set 'make-rat '(rational) (lambda (a b) (set-tag 'rational (make-rat a b))))
    (table-set 'numer '(rational) numer)
    (table-set 'denom '(rational) denom)
    (table-set 'add '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (add-rat rat1 rat2))))
    (table-set 'sub '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (sub-rat rat1 rat2))))
    (table-set 'mul '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (mul-rat rat1 rat2))))
    (table-set 'div '(rational rational) (lambda (rat1 rat2) (set-tag 'rational (div-rat rat1 rat2))))
    (table-set 'eq-num? '(rational rational) (lambda (rat1 rat2) (eq-num? rat1 rat2)))

        'done)


;;; REAL NUMBER PACKAGE

(define (install-real-num-package)
    (define (make-real-num x) (set-tag 'real-num x))
    (table-set 'make-real-num '(real-num) (lambda (n) (make-real-num n)))
    (table-set 'eq-num? '(real-num real-num) (lambda (n m) (< (- n m) tolerance)))
        'done)


;;; COMPLEX NUMBERS PACKAGE

(define (install-complex-package)
    ;; RECTANGULAR SUBPACKAGE
     (define (install-rectangular-package)
          ;; constructor and accessor code
        (define (make-from-real-imag a b) (cons a b))
        (define (real-part z) (car z))
        (define (imag-part z) (cdr z)) 
        (define (magnitude z) (sqrt (+ (* (real-part z) (real-part z)) (* (imag-part z) (imag-part z)))))
        (define (angle z) (atan (imag-part z) (real-part z)))

          ;; registration code
        (table-set 'make-from-real-imag '(rect) (lambda (a b) (set-tag 'rect (make-from-real-imag a b))))
        (table-set 'real-part '(rect) real-part)
        (table-set 'imag-part '(rect) imag-part)
        (table-set 'magnitude '(rect) magnitude)
        (table-set 'angle '(rect) angle)
               
                  'done)

    ;; POLAR SUBPACKAGE
    (define (install-polar-package)
          ;; constructor and accessor code 
        (define (make-from-mag-ang a b) (cons a b))
        (define (magnitude z) (car z))
        (define (angle z) (cdr z)) 
        (define (real-part z) (* (magnitude z) (cos (angle z))))
        (define (imag-part z) (* (magnitude z) (sin (angle z))))

           ;; registration code 
        (table-set 'make-from-mag-ang '(polar) (lambda (a b) (set-tag 'polar (make-from-mag-ang a b))))
        (table-set 'real-part '(polar) real-part)
        (table-set 'imag-part '(polar) imag-part)
        (table-set 'magnitude '(polar) magnitude)
        (table-set 'angle '(polar) angle)
           
              'done)
    
    ;; Adding packages for use in rest of complex package
    (install-rectangular-package)
    (install-polar-package)

    ;; constructor code
    (define (make-complex-from-real-imag a b) (apply (table-get 'make-from-real-imag '(rect)) (list a b)))
    (define (make-complex-from-mag-ang r theta) (apply (table-get 'make-from-mag-ang '(polar)) (list r theta)))
    
    ;; defining for use in the level two procedures (only for complex here)
    (define (apply-generic op . args) (apply (table-get op (list (get-tag (car args)))) (map get-contents args)))
    (define (real-part z) (apply-generic 'real-part z))
    (define (imag-part z) (apply-generic 'imag-part z))
    (define (magnitude z) (apply-generic 'magnitude z))
    (define (angle z) (apply-generic 'angle z))

    ;; level two procedures
    (define (add z1 z2) (make-complex-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2))))
    (define (sub z1 z2) (make-complex-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))
    (define (mul z1 z2) (make-complex-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2))))
    (define (div z1 z2) (make-complex-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))
    (define (eq-num? z1 z2) (and (< (- (real-part z1) (real-part z2)) tolerance) (< (- (real-part z1) (real-part z2)) tolerance)))

    ;; registration code 
    (table-set 'make-complex-from-real-imag '(complex) (lambda (a b) (set-tag 'complex (make-complex-from-real-imag a b))))
    (table-set 'make-complex-from-mag-ang '(complex) (lambda (r theta) (set-tag 'complex (make-complex-from-mag-ang r theta))))
    (table-set 'magnitude '(complex) magnitude)
    (table-set 'angle '(complex) angle)
    (table-set 'real-part '(complex) real-part)
    (table-set 'imag-part '(complex) imag-part)
    (table-set 'add '(complex complex) (lambda (z1 z2) (set-tag 'complex (add z1 z2))))
    (table-set 'sub '(complex complex) (lambda (z1 z2) (set-tag 'complex (sub z1 z2))))
    (table-set 'mul '(complex complex) (lambda (z1 z2) (set-tag 'complex (mul z1 z2))))
    (table-set 'div '(complex complex) (lambda (z1 z2) (set-tag 'complex (div z1 z2))))
    (table-set 'eq-num? '(complex complex) (lambda (z1 z2) (eq-num? z1 z2)))


        'done)

;;; INSTALLING PACKAGES
(install-integer-package)
(install-rational-package)
(install-real-num-package)
(install-complex-package)

;;; OUTSIDE CONSTRUCTERS (FOR lisp-number, rational, complex rect and complex polar)
(define (make-integer a) ((table-get 'make-integer '(integer)) a))
(define (make-rat numer denom) (apply (table-get 'make-rat '(rational)) (list numer denom)))
(define (make-real-num a) ((table-get 'make-real-num '(real-num)) a))
(define (make-complex-from-real-imag a b) (apply (table-get 'make-complex-from-real-imag '(complex)) (list a b)))
(define (make-complex-from-mag-ang r theta) (apply (table-get 'make-complex-from-mag-ang '(complex)) (list r theta)))

;;; RAISING FUNCTIONS
;; defining the functions
(define (raise-integer-to-rational x) (apply (table-get 'make-rat '(rational)) (list (cdr x) 1)))
(define (raise-rational-to-real x) ((table-get 'make-real-num '(real-num)) (/ (cadr x) (cddr x))))
(define (raise-real-to-complex x) (apply (table-get 'make-complex-from-real-imag '(complex)) (list (cdr x) 0)))
;; registering functions under 'raise
(table-set 'raise '(integer) (lambda (x) (raise-integer-to-rational x)))
(table-set 'raise '(rational) (lambda (x) (raise-rational-to-real x)))
(table-set 'raise '(real-num) (lambda (x) (raise-real-to-complex x)))

;;; PROJECTION FUNCTIONS (opposite of raising)
(define (project-rational-to-integer x) (make-integer (round (/ ((table-get 'numer '(rational)) (get-contents x)) ((table-get 'denom '(rational)) (get-contents x))))))
(define (project-real-to-rational x) (make-rat (round (cdr x)) 1))
(define (project-complex-to-real x) (make-real-num ((table-get 'real-part '(complex)) x)))
;; registering functions under 'proj
(table-set 'project '(rational) (lambda (x) (project-rational-to-integer x)))
(table-set 'project '(real-num) (lambda (x) (project-real-to-rational x)))
(table-set 'project '(complex) (lambda (x) (project-complex-to-real x)))


;;; LOWER FUNCTION
(define (lower num-obj)
    ;; checks if safe to lower number type, if so, lowers it, otherwise, returns number; safe if (raise (project num)) = num
    (if (equal? (car type-tower) (get-tag num-obj))
        num-obj
        (if (equal? ((table-get 'raise (list (get-tag ((table-get 'project (list (get-tag num-obj))) num-obj)))) ((table-get 'project (list (get-tag num-obj))) num-obj)) num-obj)
            (lower ((table-get 'project (list (get-tag num-obj))) num-obj))
            num-obj)))


;;; APPLY GENERIC 

(define (apply-generic op . args)
    ; for telling which arg to raise
    (define (get-tower-tier num-obj)
        ; raises level until it gets to complex (tier 4 is integer, tier 3 is rational, tier 2 is real, tier 1 is complex)
        (if (equal? (get-tag num-obj) 'complex)
            1
            (+ 1 (get-tower-tier ((table-get 'raise (list (get-tag num-obj))) num-obj))))) ; raises to next level

    ; converts args to the same type and then applies the correct function from the table and then lowers the value
    (lower
      (cond ((not (equal? 'none (table-get op (map get-tag args))))
            (apply (table-get op (map get-tag args)) (map get-contents args)))
          ((> (get-tower-tier (car args)) (get-tower-tier (cadr args))) (apply-generic op ((table-get 'raise (list (get-tag (car args)))) (car args)) (cadr args)))
          ((< (get-tower-tier (car args)) (get-tower-tier (cadr args))) (apply-generic op (car args) ((table-get 'raise (list (get-tag (cadr args)))) (cadr args))))
          ((= (get-tower-tier (car args)) (get-tower-tier (cadr args))) (if (and (equal? (get-tag (car args)) 'complex) (equal? (get-tag (car args)) 'complex))
                                                                          (error "apply-generic does not work with current inputs")
                                                                          (apply-generic op ((table-get 'raise (list (get-tag (car args)))) (car args)) (cadr args))))
          (else (error "apply-generic does not work with current inputs")))
      )
    )




;; level 2 fucntions with apply-generic
(define (add n m) (apply-generic 'add n m))
(define (subtract n m) (apply-generic 'sub n m))
(define (multiply n m) (apply-generic 'mul n m))
(define (divide n m) (apply-generic 'div n m))
(define (eq-num? n m) (apply-generic 'eq-num? n m))

;test code
;(define a (make-integer 6))
;(define b ((table-get 'raise '(integer)) a))
;(define c ((table-get 'raise '(rational)) b))
;(define d ((table-get 'raise '(real-num)) c))


;(define test1 (make-rat 1 5))
;(define test2 (make-complex-from-mag-ang 1 1.57))
;(add test1 test2)
;(subtract test1 test2)
;(multiply test1 test2)
;(divide test1 test2)

;;; END: 2.38


;;; BEGIN: 2.39

; One way to implement this generalized apply-generic is to perform the operation on the first two arguments by raising appropriately, and then performing the operation on the result of that and the next argument, and repeat.
; One such case where the given algorithm does not work is when none of the tags of the arguments have direct implementations of the operation within the table, so apply generic will never be abl to look up the fucnctino progerply.
; ('real-num 5) ('real-num 5), when attempting to add together will not find a function with the given algorithm

;;; END: 2.39
