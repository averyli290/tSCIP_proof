;;; BEGIN: 1.0

(define (my-plus x y) (+ x y))

;;; END: 1.0


;;; BEGIN: 1.1

10
12
8
3
6
a
b
19
#f
4
16
6
16

;;; END: 1.1


;;; BEGIN: 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;;; END: 1.2


;;; BEGIN: 1.3

(define (harriet a b)
    (if (> (+ (/ 1 a) (/ 1 b)) 0 )
        (+ (/ 1 a) (/ 1 b))
        0 ))

;;; END: 1.3


;;; BEGIN: 1.4

The new/different thing with this definition is that there are procedures that are outputs, which are then applied to a and b. It's... intriguing.

;;; END: 1.4


;;; BEGIN: 1.5

In a "special" function, not all of the inputs are necessarily evaluted, while in our newly created one, that isn't the case. If the second else-clause cannot be evaluated, it will work in the original if, but not in our created one.

;;; END: 1.5


;;; BEGIN: 1.6

(define (triangular n)
    (if (> n 0)
        (+ n (triangular (- n 1)))
        0))

;;; END: 1.6


;;; BEGIN: 1.7

The first process is recursive because it stores values to later be incremented.
The second process is iterative because it just returns the value of the next iteration without any modifications.

;;; END: 1.7



;;; BEGIN: 1.8

This type of process takes exponential time and exponential space.
It takes exponential time because in each iteration, it splits off into two branches.
It takes exponetial space because it stores 2 values each iteration.

;;; END: 1.8


;;; BEGIN: 1.9

(define (fib-it n)
    (define (iter counter value-1 value-2)
        (if (= counter n)
            value-1
            (iter (+ 1 counter) value-2 (+ value-1 value-2))))

    (iter 0 0 1))

;;; END: 1.9


;;; BEGIN: 1.10

(define (exp b n)
    (if (= 0 n)
        1
        (* b (exp b (- n 1)))))

(define (exp-it b n)
    (define (iter accum counter)
        (if (= counter 0)
            accum
            (iter (* accum b) (- counter 1))))
    (iter 1 n))

;;; END: 1.10


;;; BEGIN: 1.11

(define (square n)
    (* n n))

(define (fast-exp b n)
    (cond ((= n 1) b)
        ((even? n) (square (fast-exp b (/ n 2))))
        ((even? (- n 1)) (* b (fast-exp b (- n 1)))))) 

;;; END: 1.11


;;; BEGIN: 1.12

(define (fast-exp-it b n)
    (define (iter total current-base counter)
        (if (= counter 0)
            total
            (if (even? counter)
                (iter total (square current-base) (/ counter 2))
                (iter (* total current-base) current-base (- counter 1)))))

    (iter 1 b n))

;;; END: 1.12


;;; BEGIN: 1.13

(define (pi-rec n)
    (if (= n 1) 
        (/ 1.0 3)
        (+ (/ 1.0 (* (- (* 2 (+ n 1)) 1) (+ (* 2 (+ n 1)) 1))) (pi-rec (- n 1)))))

(define (pi-it n)
    (define (iter a c)
        (if (<= c 4)
            (+ a (/ 1.0 3))
            (iter (+ a (/ 1.0 (* (- c 1) (+ c 1)))) (- c 2))))
    (iter 0 (* 2 n)))

;;; END: 1.13


;;; BEGIN: 1.14

(define (make-adder n)
    (lambda (x) (+ x n)) )

;;; END: 1.14


;;; BEGIN: 1.15

2
6
error

;;; END: 1.15


;;; BEGIN: 1.16

((lambda (var-1 var-2 ... var-n)
    (body-1) (body-2) ... (body-m))
    (val-1 val-2 ... val-n))

;;; END: 1.16


;;; BEGIN: 1.17

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (incr x) (+ x 1))
(define (identity x) x)

(define (sum-rec a b term-fn)
    (if (> a b)
        0
        (+ (term-fn a) (sum-rec (+ a 1) b term-fn))))

(define (sum-it a b term-fn)
    (define (iter accum counter)
        (if (> counter b)
            accum
            (iter (+ accum (term-fn counter)) (+ counter 1))))
    (iter 0 a))

;;; END: 1.17


;;; BEGIN: 1.18

(define (accumulate-rec a b start-value combine-fn term-fn next-fn)
    (if (> a b)
        start-value
        (combine-fn (term-fn a)
            (accumulate-rec (next-fn a) b start-value combine-fn term-fn next-fn))
        )
    )

(define (accumulate-it a b start-value combine-fn term-fn next-fn)
    (define (iter accum counter)
        (if ( (if (< a b) > <) counter b)
            accum
            (iter (combine-fn accum (term-fn counter)) (next-fn counter)))
        )
    (iter start-value a) 
    )

;;; END: 1.18
;;; BEGIN: 1.19

(accumulate-it 5 100 (/ (* 2 4) (* 3 3.0)) * 
               (lambda (n) (/ (* (- n 1) (+ n 1)) (* n n)))
               (lambda (n) (+ n 2)))

;;; END: 1.19
;;; BEGIN: 1.20

(define (integrate f a b n)
    (accumulate-it a b 0 +
            (lambda (x) (* (/ (f x) n) (- b a)))
            (lambda (x) (+ x (/ (- b a) n)))
    )
)

;;; END: 1.20
;;; BEGIN: 1.21



;;; END: 1.21
;;; BEGIN: 1.22

(cons (cons 1 (cons 2 '())) (cons 3 (cons 4 '())))

(list (list 1 2) 3 4)

;;; END: 1.22
;;; BEGIN: 1.23

cdddr will take the cdr of the cdr of the cdr of the pair.
In general it will take the cdr or car depending on whether it is a d or a, respectively from right to left.

;;; END: 1.23
;;; BEGIN: 1.24

(define (my-length l)
    (define (iter lst len)
        (if (null? lst)
            len
            (iter (cdr lst) (+ len 1))
            )
        )
    (iter l 0)
    )

; (define l (list 1 2 3 4 5))
; (my-length l)

;;; END: 1.24
;;; BEGIN: 1.25

(define (range n)
    (define (iter counter accum)
        (if (= counter -1)
            accum
            (iter (- counter 1) (cons counter accum))
            )
        )
    (iter (- n 1) '())
    )

; (range 5)

;;; END: 1.25
;;; BEGIN: 1.26

(define (new-cons x y)
    (define (dispatch m)
        (cond ((= m 0) x)
                ((= m 1) y)))
    dispatch)

(define (new-car p)
    (p 0))

(define (new-cdr p)
    (p 1))

; (define a (new-cons 5 6))
; (define b (new-car a))
; (define c (new-cdr a))

; b
; c

;;; END: 1.26
;;; BEGIN: 1.27

(define (new-cons-2 x y)
    (lambda (m) (m x y)))

(define (new-car-2 p)
    (p (lambda (x y) x)))

(define (new-cdr-2 p)
    (p (lambda (x y) y)))

; (define a (new-cons-2 1 2))
; (define b (new-car-2 a))
; (define c (new-cdr-2 a))
; b
; c

;;; END: 1.27
;;; BEGIN: 1.28


;;; END: 1.28
;;; BEGIN: 1.29

(define (my-reverse l)
    (define (iter new old)
        (if (null? old)
            new
            (iter (cons (car old) new) (cdr old))
            )
        )
    (iter '() l)
    )

; (define test (list 1 2 3 4))
; (my-reverse test)

;;; END: 1.29
;;; BEGIN: 1.30

(define (last-elt l)
    (if (null? l)
        (error "empty list"))
    (if (null? (cdr l))
        (car l)
        (last-elt (cdr l))
        )
    )

; (define test (list 1 2 3 4))
; (last-elt test)

;;; END: 1.30
;;; BEGIN: 1.31

(define (scale-2 numbers)
    (if (null? numbers)
        '()
        (cons (* (car numbers) 2) (scale-2 (cdr numbers)))
        )
    )

; (define test (list 1 2 3 4))
; (scale-2 test)

;;; END: 1.31
;;; BEGIN: 1.32

(define (my-map numbers f)
    (if (null? numbers)
        '()
        (cons (f (car numbers)) (my-map (cdr numbers) f))
        )
    )
;; Test
; (define test (list 1 2 3 4))
; (my-map test (lambda (x) (* x x)))

;;; END: 1.32
;;; BEGIN: 1.33

The numbers are added to the list backwards.

;;; END: 1.33
;;; BEGIN: 1.34

It displays with the wrong dot notation.

;;; END: 1.34
;;; BEGIN: 1.35

(define (my-append l1 l2)
    (define (iter l1 l2 accum)
        (if (null? l1)
            (if (null? l2)
                accum
                (iter l1 (cdr l2) (cons (car l2) accum))
                )
            (iter (cdr l1) l2 (cons (car l1) accum))
            )
        )

    (define (my-reverse l)
        (define (iter new old)
            (if (null? old)
                new
                (iter (cons (car old) new) (cdr old))))
        (iter '() l))

    (my-reverse (iter l1 l2 '()))
    )

; (define l1 (list 1 2 3 4))
; (define l2 (list 5 6 7 8))

; (my-append l1 l2)


;;; END: 1.35
;;; BEGIN: 1.36

(define (slice l n m)
    (if (= n m)
        (cons (car l) '())
        (if (> n 0)
            (slice (cdr l) (- n 1) (- m 1)) 
            (cons (car l) (slice (cdr l) n (- m 1)))
            )
        )
    )

; (define test (list 1 2 3 4 5 6 7))
; (slice test 0 6)

;;; END: 1.36
;;; BEGIN: 1.37

(define (accumulate start-value combine-fn l)
    (if (null? (cdr l))
        (combine-fn (car l) start-value)
        (combine-fn (car l) (accumulate start-value combine-fn (cdr l)))
        ) 
    )

; (accumulate 0 + (list 1 2 3))
; (accumulate '() cons (list 1 2 3))

;;; END: 1.37
;;; BEGIN: 1.38

(define (length-via-acc l)
    (accumulate 0 (lambda (x y) (+ y 1)) l))

(define (map-via-acc l f)
    (accumulate '() (lambda (x y) (cons (f x) y)) l))

(define (append-via-acc l1 l2)
    (accumulate l2 (lambda (x y) (cons x y)) l1))

; (define test1 (list 1 2 3 4 5))
; (define test2 (list 6 7 8 9 10))
; (length-via-acc test1)
; (map-via-acc test1 (lambda (x) (* x x)))
; (append-via-acc test1 test2)

;;; END: 1.38
;;; BEGIN: 1.39

(1 (2 (3 4)))
_____
|1|||
``_v`
  |2|||
  ```|`
    _v___
    |3|-|
    ```|`
      _v___
      |4|/|

1
|\
2 *
  |\
  3 4

;;; END: 1.39
;;; BEGIN: 1.40

;(define test1 '(1 3 (5 7) 9))
;(define test2 '((7)))
;(define test3 '(1 (2 (3 (4 (5 (6 7)))))))
(car (cdaddr test1))
(caar test2)
(cadadr (cadadr (cadadr test3)))


;;; END: 1.40
;;; BEGIN: 1.41

;(define test1 '(1 3 (5 7) 9))
;(define test2 '((7)))
;(define test3 '(1 (2 (3 (4 (5 (6 7)))))))
(car (cdaddr test1))
(caar test2)
(cadadr (cadadr (cadadr test3)))

;;; END: 1.41
;;; BEGIN: 1.42

;(define x (list 1 2 3))
;(define y (list 4 5 6))

a) (1 2 3 4 5 6) 
b) ((1 2 3) 4 5 6)
c) ((1 2 3) (4 5 6))

;;; END: 1.42
;;; BEGIN: 1.43

(define (depth t)
    (cond ((null? (cdr t)) 0)
          ((pair? t) (max (+ 1 (depth (cadr t))) (depth (cdr t)))) 
          (else (depth (cdr t)))
          )
    )

;(define test '(1 (2) (3 (4)) (5 (6 (7)))))
;(depth test)

;;; END: 1.43
;;; BEGIN: 1.44

(define (deep-reverse t)
    (define (iter tree accum)
        (cond ((null? tree) accum)
              ((pair? (car tree)) (iter (cdr tree) (cons (deep-reverse (car tree)) accum)))
              (else (iter (cdr tree) (cons (car tree) accum)))
              )
        )
    (iter t '())
    )

;(define test (list (list 1 2) (list 3 4)))
;(deep-reverse test)

;;; END: 1.44
;;; BEGIN: 1.45

(define (enumerate t)
(if (null? t)
t
(if (pair? (car t))
(append (enumerate (car t)) (enumerate (cdr t)))
(append (list (car t)) (enumerate (cdr t)))
)
)
)

(define (enumerate-2 t)
(define (iter new old)
(if (null? old)
new
(if (pair? old)
(iter (iter new (car old)) (cdr old))
(cons old new)
)
)
)
(reverse (iter '() t))
)

;;; END: 1.45
;;; BEGIN: 1.46

;(fold-right (lambda (x y) (+ (* 2 x) (* 3 y))) 1 '(1 2 4))

(define (accumulate start-value combine-fn l)
    (if (null? (cdr l))
        (combine-fn (car l) start-value)
        (combine-fn (car l) (accumulate start-value combine-fn (cdr l)))
        ) 
    )

(define (count-leaves tree)
    (accumulate 0 + (map (lambda (x) (if (not (pair? x))
                                        1 
                                        (+ (count-leaves (list (car x))) (count-leaves (cdr x)))
                                      )
                        )
                       tree
                    ) 
                )
    )

;(define test '(1 2 (3 4 5) (6 7)))
;(count-leaves test)

;;; END: 1.46
;;; BEGIN: 1.47

(define (map-tree t f)
    (cond ((null? t) '())
        ((pair? t) (cons (map-tree (car t) f) (map-tree (cdr t) f)))
        (else (f t))
        )
    )


;(define test (list 1 2 (list 3 4) (list 5 6)))
;(map-tree test (lambda (x) (+ 1 x)))

;;; END: 1.47
;;; BEGIN: 1.48

(define (fold-left f start-value l)
    (define (iter l)
        (if (null? (cdr l))
            car (l)
            (f (car l) (iter (cdr l)))
            )
        )
    (f start-value (iter l))
    )

;(fold-left - 0 '(1 2 3))

;;; END: 1.48
;;; BEGIN: 1.49

(define (my-reverse-left l)
    (fold-left (lambda (x y) (cons y x))
               '()
               l)
    )

;(define test1 (list 1 2 3 4))
;(my-reverse test1)

(define (my-reverse-right l)
    (fold-right (lambda (x y) (cons x))
                '()
                l)
    )

;(define test2 (list 1 2 3 4))
;(my-reverse-right test2)

;;; END: 1.49
;;; BEGIN: 1.51


(define (good-enough? guess x)
    (< (abs (- (* guess guess) x)) tolerance))

(define (next-guess guess x)
    (/ (+ guess (/ x guess)) 2))

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (next-guess guess x) x))
    )

(define (sqrt x)
    (sqrt-iter 2.0 x))

;;; END: 1.51
;;; BEGIN: 1.52

(define (half-interval-method f a b)
    (let (
          (w (/ (+ a b) 2))
          )
        (cond ((< (abs (f w)) tolerance) (+ w 0.0))
              ((< (f w) 0) (half-interval-method f w b))
              (else (half-interval-method f a w))
              )
        )
    )


(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1 2)

;;; END: 1.52
;;; BEGIN: 1.53

(define (fixed-point f initial-guess)
    (if (< (abs (- (f initial-guess) initial-guess)) tolerance)
        (f initial-guess)
        (begin (display (f initial-guess)) (newline) (fixed-point f (f initial-guess)))
        )
    )


(fixed-point (lambda (y) (+ (sin y) (cos y))) 1)

;;; END: 1.53
;;; BEGIN: 1.54

; We have that x^2=n
; therefore our function is n/x=x


(define (fixed-point-sqrt x)
    (fixed-point (lambda (t) (/ t x))
                 2)
    )

;(fixed-point-sqrt 10.0)

;the value jumps around and doesn't get a correct value

;;; END: 1.54
;;; BEGIN: 1.55

(define (fixed-point f initial-guess)
    (if (< (abs (- (f initial-guess) initial-guess)) tolerance)
        (f initial-guess)
        (begin (display (f initial-guess)) (newline) (fixed-point f (f initial-guess)))
        )
    )


(fixed-point (lambda (x) (/ (+ x (/ 10 x)) 2))
             10.0)

;;; END: 1.55
;;; BEGIN: 1.56

(define (fixed-point f initial-guess)
    (if (< (abs (- (f initial-guess) initial-guess)) tolerance)
        (f initial-guess)
        (begin (display (f initial-guess)) (newline) (fixed-point f (f initial-guess)))
        )
    )


(fixed-point (lambda (x) (/ (log 1000) (log x))) 5)
; 27 iterations
(display "===break===")
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 5) 
; 7 iterations

;;; END: 1.56
;;; BEGIN: 1.57

(define (average-damping f)
    (lambda (x) (/ (+ x (f x)) 2)))


(define (fixed-point-from-transform f transform initial-guess)
    (if (< (abs (- ((transform f) initial-guess) initial-guess)) tolerance)
        ((transform f) initial-guess)
        (fixed-point-from-transform f transform ((transform f) initial-guess))
        
      )
    )

(fixed-point-from-transform (lambda (w) (/ 100 (* w w w)))
                            (lambda (f) (average-damping (average-damping f)))
                            9.0)


;;; END: 1.57
;;; BEGIN: 1.58

(define (fixed-point-from-transform f transform initial-guess)
    (if (< (abs (- ((transform f) initial-guess) initial-guess)) tolerance)
        ((transform f) initial-guess)
        (fixed-point-from-transform f transform ((transform f) initial-guess))
        
      )
    )

;(fixed-point-from-transform (lambda (w) (/ 1000 (* w w))) average-damping 9.0)

(define (comp f g)
    (lambda (x) (f (g x))))

(define (nth-power f n)
    (if (= n 0)
        (lambda (x) x)
        (comp f (nth-power f (- n 1)))))

(define (nth-root n r)

    (fixed-point-from-transform (lambda (w) (/ n (exp w (- r 1))))
                                (nth-power average-damping (ceiling (/ (log r) (log 2))))
                                9)
    )


(nth-root 100 2)

;;; END: 1.58