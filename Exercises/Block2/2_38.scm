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
    ;; checks if safe to lower number type, if so, lowers it; safe if (raise (project num)) = num
    (if (equal? (car type-tower) (get-tag num-obj))
        num-obj
        (if (equal? ((table-get 'raise (list (get-tag ((table-get 'project (list (get-tag num-obj))) num-obj)))) ((table-get 'project (list (get-tag num-obj))) num-obj)) num-obj)
            (lower ((table-get 'project (list (get-tag num-obj))) num-obj)))))



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
(define a (make-integer 6))
(define b ((table-get 'raise '(integer)) a))
(define c ((table-get 'raise '(rational)) b))
(define d ((table-get 'raise '(real-num)) c))


