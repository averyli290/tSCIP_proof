(define test-1 (list 1 2 3))
(define test-2 test-1)
test-1
test-2
(define test-1 (cdr test-1))
test-1
test-2
