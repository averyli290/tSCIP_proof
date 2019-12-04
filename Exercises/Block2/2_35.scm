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
        (define (angle z) (atan (real-part z) (imag-part z)))

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

