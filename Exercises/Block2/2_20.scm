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

