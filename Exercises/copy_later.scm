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


;;; BEGIN 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;;; END 1.2


;;; BEGIN 1.3

(define (harriet a b)
    (if (> (+ (/ 1 a) (/ 1 b)) 0 )
        (+ (/ 1 a) (/ 1 b))
        0 ))

;;; END 1.3


;;; BEGIN 1.4

The new/different thing with this definition is that there are procedures that are outputs, which are then applied to a and b. It's... intriguing.

;;; END 1.4


;;; BEGIN 1.5

In a "special" function, not all of the inputs are necessarily evaluted, while in our newly created one, that isn't the case. If the second else-clause cannot be evaluated, it will work in the original if, but not in our created one.

;;; END 1.5


;;; BEGIN 1.6

(define (triangular n)
    (if (> n 0)
        (+ n (triangular (- n 1)))
        0))

;;; END 1.6


;;; BEGIN 1.7

The first process is recursive because it stores values to later be incremented. The second process is iterative because it just returns the value of the next iteration without any modifications.

;;; END 1.7



;;; BEGIN 1.8

This type of process takes quadratic time and linear space.

;;; END 1.8


;;; BEGIN 1.9

(define (fib-it n)
    (define (iter counter value-1 value-2)
        (if (= counter n)
            value-1
            (iter (+ 1 counter) value-2 (+ value-1 value-2))))

    (iter 0 0 1))

;;; END 1.9


;;; BEGIN 1.10

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

;;; END 1.10


;;; BEGIN 1.11

(define (square n)
    (* n n))

(define (fast-exp b n)
    (cond ((= n 1) b)
        ((even? n) (square (fast-exp b (/ n 2))))
        ((even? (- n 1)) (* b (fast-exp b (- n 1)))))) 

;;; END 1.11


;;; BEGIN 1.12

(define (fast-exp-it b n)
    (define (iter total current-base counter)
        (if (= counter 0)
            total
            (if (even? counter)
                (iter total (square current-base) (/ counter 2))
                (iter (* total current-base) current-base (- counter 1)))))

    (iter 1 b n))

;;; END 1.12


;;; BEGIN 1.13

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

;;; END 1.13


;;; BEGIN 1.14

(define (make-adder n)
    (lambda (x) (+ x n)) )

;;; END 1.14


;;; BEGIN 1.15

2
6
error

;;; END 1.15


;;; BEGIN 1.16

((lambda (var-1 var-2 ... var-n)
    (body-1) (body-2) ... (body-m))
    (val-1 val-2 ... val-n))

;;; END 1.16


;;; BEGIN 1.17

(define (square x) (* x x))
(define (cube x) (* x x x))
(define (incr x) (+ x 1))
(define (identity x) x)

(define (sum-rec a b term-fn)
    (if (= a b)
        (term-fn a)
        (+ (term-fn a) (sum-rec (+ a 1) b term-fn))))

(define (sum-it a b term-fn)
    (define (iter accum counter)
        (if (= counter b)
            (+ accum (term-fn counter))
            (iter (+ accum (term-fn counter)) (+ counter 1))))
    (iter 0 a))

;;; END 1.17


;;; BEGIN 1.18

(define (accumulate-rec a b start-value combine-fn term-fn next-fn)
    (if (= a b)
        (combine-fn (term-fn b) start-value)
        (combine-fn (term-fn a)
            (accumulate-rec (next-fn a) b start-value combine-fn term-fn next-fn))
        )
    )

(define (accumulate-it a b start-value combine-fn term-fn next-fn)
    (define (iter accum counter)
        (if (= counter b)
            (combine-fn accum (term-fn counter))
            (iter (combine-fn accum (term-fn counter)) (next-fn counter)))
        )
    (iter start-value a) 
    )

;;; END 1.18


;;; BEGIN 1.19

(accumulate-it 5 100 (/ (* 2 4) (* 3 3.0)) * 
               (lambda (n) (/ (* (- n 1) (+ n 1)) (* n n)))
               (lambda (n) (+ n 2)))

;;; END 1.19


;;; BEGIN 1.20

(define (integrate f a b n)
    (accumulate-it a b a +
            (lambda (x) (/ (f x) n))
            (lambda (x) (+ x (/ (- b a) n))) )
    )

(integrate cube 0 1 1000.0)

;;; END 1.20

