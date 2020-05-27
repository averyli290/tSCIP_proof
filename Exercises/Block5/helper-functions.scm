;;; Precompilation for handling cons, car, cdr.
;;;
;;; Takes sugared RML with CONS, CAR, CDR as ops
;;; in the form of a list of lists;
;;; returns expanded RML that uses subroutines.
;;;
;;; For example:
;;; '((assign (reg a) (const 3))
;;; (assign (reg val) CONS (const 5) (reg a)))
;;;
;;; returns:
;;; '((assign (reg a) (const 3))
;;; (assign (reg p-1) (const 5))
;;; (assign (reg p-2) (reg a))
;;; (assign (reg p-cont) (label precompile-1))
;;; (goto cons)
;;; precompile-1
;;; (assign (reg val) (reg p-val)))
(define label-index 0)

(define (main-map line)
    (cond ((or (not (pair? line)) (< (length line) 3))
          (list line))
          ((eq? 'CONS (caddr line))
          (make-cons line))
          ((eq? 'CAR (caddr line))
          (make-car line))
          ((eq? 'CDR (caddr line))
          (make-cdr line))
          ((eq? 'CADR (caddr line))
          (make-cadr line))
          ((eq? 'CADDR (caddr line))
          (make-caddr line))
          ((eq? 'CADDDR (caddr line))
          (make-cadddr line))
          ((eq? 'CDDR (caddr line))
          (make-cddr line))
          (else
          (list line))))

(define (make-cons line)
    (set! label-index (+ 1 label-index))
    (let ((dest (cadr line))
          (source-1 (cadddr line))
          (source-2 (cadddr (cdr line)))
          (label (symbol-append 'precompile- label-index)))
    (list (list 'assign '(reg p-1) source-1)
          (list 'assign '(reg p-2) source-2)
          (list 'assign '(reg p-cont) (list 'label label))
          '(goto (label cons))
          label
          (list 'assign dest '(reg p-val)))))


; CAR: 
; (assign (reg a) CAR (reg b))
; turns into:
; (assign (reg p-1) (reg b))
; (assign (reg p-cont) (label precompile-x))
; (goto (label car))
; precompile-x
; (assign (reg a) (reg p-val))
(define (make-car line)
    (set! label-index (+ 1 label-index))
    (let ((dest (cadr line))
        (source (cadddr line))
        (label (symbol-append 'precompile- label-index)))
    (list (list 'assign '(reg p-1) source)
          (list 'assign '(reg p-cont) (list 'label label))
          (list 'goto '(label car))
          label
          (list 'assign dest '(reg p-val)))))


(define (make-cdr line)
    (set! label-index (+ 1 label-index))
    (let ((dest (cadr line))
        (source (cadddr line))
        (label (symbol-append 'precompile- label-index)))
    (list (list 'assign '(reg p-1) source)
          (list 'assign '(reg p-cont) (list 'label label))
          (list 'goto '(label cdr))
          label
          (list 'assign dest '(reg p-val)))))

;;; oh, I'm so clever...
(define (make-cadr line)
    (let ((dest (cadr line))
        (source (cadddr line)))
    (list (list 'assign dest 'CDR source)
          (list 'assign dest 'CAR dest))))

(define (make-caddr line) 
    (let ((dest (cadr line))
        (source (cadddr line)))
    (list (list 'assign dest 'CDR source)
          (list 'assign dest 'CDR dest)
          (list 'assign dest 'CAR dest))))

(define (make-cadddr line) 
    (let ((dest (cadr line))
        (source (cadddr line)))
    (list (list 'assign dest 'CDR source)
          (list 'assign dest 'CDR dest)
          (list 'assign dest 'CDR dest)
          (list 'assign dest 'CAR dest))))

(define (make-cddr line) 
    (let ((dest (cadr line))
        (source (cadddr line)))
    (list (list 'assign dest 'CDR source)
          (list 'assign dest 'CDR dest))))

(define (loop-once RML)
    (fold-left append '() (map main-map RML)))

(define (precompile-RML RML)
    (loop-once (loop-once RML)))


;;; printing vectors
(define (print-vector v start end)
    (if (= start end)
        '.
        (begin (display start) (display ":") (display (vector-ref v start)) (display "  ") (print-vector v (+ 1 start) end))))


;; Convert a nested list of list of... into vector format.
;; Input f is the current free index.
;; Returns the next free index.
(define (lisp-to-vectors obj f v-cars v-cdrs)
    (define (setup-cars obj f)
        (cond ((not (pair? (car obj)))
                (vector-set! v-cars f (car obj))
                (+ 1 f))
              (else
                (vector-set! v-cars f (make-pointer (+ 1 f)))
                (lisp-to-vectors (car obj) (+ 1 f) v-cars v-cdrs))))

    (cond ((not (pair? (cdr obj)))
            (vector-set! v-cdrs f (cdr obj))
            (setup-cars obj f))
          (else
            (let ((new-free (setup-cars obj f))) ; do cars before cdrs
            (vector-set! v-cdrs f (make-pointer new-free))
            (lisp-to-vectors (cdr obj) new-free v-cars v-cdrs)))))








