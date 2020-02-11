(load "exercise_3_14.scm")

(define (driver-loop)
    (prompt-for-input input-prompt)
    (let ((input (read)))
        (let ((output (new-eval input global)))
            (announce-output output-prompt)
            (announce-output (sanitize-output output))))
    (driver-loop))

(define input-prompt ";;; Input:")
(define output-prompt ";;; Output:")
(define (prompt-for-input string)
    (newline) (newline) (display string) (newline))
(define (announce-output string)
    (newline) (display string) (newline))

(driver-loop)

;;; code goes here




(define fib (lambda (x)
    (if (= x 1)
        1
        (if (= x 0)
            1
            (+ (fib (- x 2)) (fib (- x 1)))))))

(fib 10)


