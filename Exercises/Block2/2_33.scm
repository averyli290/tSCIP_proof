
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


