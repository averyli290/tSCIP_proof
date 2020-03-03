;;; BEGIN: 4.1

(assign (reg n) (op read))
(assign (reg counter) (const 1))
(assign (reg answer) (const 1))
loop-label
(test (op >) (reg n))
(branch (done-label))
(assign (reg temp) (op +) (reg n) (const 1))
(assign (reg n) (reg temp))
(assign (reg temp2) (op *) (reg n) (reg answer))
(assign (reg answer) (reg temp2))
(goto (label loop-label))
done-label
(perform (op write) (reg answer))

;;; END: 4.1

;;; BEGIN: 4.2

(assign (reg x) (op read))
(assign (reg guess) (const 1.0))
loop-label
(test (op good-enough?) (reg guess))
(branch (label done-label))
(assign (reg temp) (op improve) (reg guess))
(assign (reg guess) (reg temp))
(goto (label done-label))
done-label
(perform (op write) (reg guess))

;;; END: 4.2

;;; BEGIN: 4.3

(assign (reg counter) (const 1))
(assign (reg n) (op read))
(assign (reg product) (const 1))
loop-label
    (test (op >) (reg counter) (reg n))
    (branch (label done-label))
    (assign (reg temp1) (op *) (reg counter) (reg product))
    (assign (reg product) (reg temp))
    (assign (reg temp2) (op +) (reg counter) (const 1))
    (assign (reg counter) (reg temp2))
    (goto (label loop-label))
done-label
    (perform (op write) (reg product))

;;; END: 4.3