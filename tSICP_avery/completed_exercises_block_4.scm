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

;;; BEGIN: 4.7

For factorial
- the input is contained within (reg n) at the beginning
- the place to go to next is contained within (reg continue) at the beginning
- the output is contained within (reg ans) at end

Should add
- the stack is not different when it leaves and comes back 

;;; END: 4.7

;;; BEGIN: 4.8

(assign (reg n) (op read))
(assign (reg continue) (label done))
(assign (reg ans) (const 0))

fib-loop
(test (op =) (reg n) (const 1))
(branch (label base-case-one))
(test (op =) (reg n) (const 0))
(branch (label base-case-zero))
(push (reg n))
(push (reg continue))
(assign (reg continue (label fib-loop)))
(assign (reg n_minus) (op -) (reg n) (const 1))
(push (reg n_minus))
(push (reg continue))
(assign (reg n) (op -) (reg n) (const 1))
(goto (label fib-loop))

post-rec
(pop (reg continue))
(pop (reg n))
(goto (reg continue))

base-case-one
(assign (reg continue) (label post-rec))
(assign (reg temp) (op +) (const 1) (reg ans))
(assign (reg ans) (reg temp))
(goto (reg continue))

base-case-zero
(assign (reg continue) (label post-rec))
(goto (reg continue))

done
(perform (op write) (reg ans))

;;; END: 4.8

