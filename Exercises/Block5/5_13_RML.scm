
(define (the-program) ; the lisp 2 program that will be loaded into the special machine
    '(
      (define + 73)
      ;(define x 6)
      (+ 1 (+ 2 3))
      ;(define x 13)
      ;(define y 12)
      ;x 
      ))


(define (get-RML)
  '(
    (assign (reg env) (op make-environment))

    ;(assign (reg temp-1) CONS (reg null) (reg null))
    ;(assign (reg temp-1) CONS (const 1) (reg null))
    ;(assign (reg temp-1) CONS (const 2) (reg temp-1))
    ;(assign (reg temp-1) CONS (const 3) (reg temp-1))

    ;(assign (reg temp-2) CONS (const 5) (reg null))
    ;(assign (reg temp-2) CONS (const 5) (reg null))
    ;(assign (reg temp-2) CONS (const 6) (reg temp-2))
    ;(assign (reg temp-2) CONS (const 7) (reg temp-2))
    
    ;(goto (label done))

    ;(assign (reg continue) (label done))
    ;(goto (label append))

    eval-loop
        (test (op null?) (reg program)) ; tests if program is null
        (branch (label done)) ; if so, goes to label done
        (assign (reg continue) (label eval-loop))
        (assign (reg exp) CAR (reg program))
        (assign (reg program) CDR (reg program))
        (goto (label eval))

    eval ; testing to see what to use to evaluate exp (expression)
        (test (op number?) (reg exp))
        (branch (label ev-self-evalutating))
        (test (op symbol?) (reg exp))
        (branch (label ev-variable))
        (assign (reg temp-var) CAR (reg exp))
        (test (op eq?) (const define) (reg temp-var))
        (branch (label ev-define))

        (goto (label ev-apply))
        ;(assign (reg temp-1) CADR (reg exp)) ; TEMPORARY for evaluating ((define x (+ 1 3) '()) x '())
        ;(assign (reg temp-2) CADDR (reg exp))
        ;(assign (reg val) (op +) (reg temp-1) (reg temp-2))
        ;(goto (reg continue))

    ev-self-evalutating
        (assign (reg val) (reg exp))
        (goto (reg continue)) ; goes to the label stored in continue after any evaluation

    ev-variable
        (assign (reg val) (perform (op lookup-var-value) (reg exp) (reg env))) ; looking up the value of the variable in the environment
        (goto (reg continue)) ; goes to the label stored in continue after any evaluation

    ev-define
        (push (reg exp)) ; saving the state of exp and continue
        (push (reg continue))
        (assign (reg exp) CADDR (reg exp)) ; getting the sub expression part of (define x sub-expr) !!! Has to be CADDR
        (assign (reg continue) (label ev-define-2)) ; setting continue
        (goto (reg eval)) ; evaluating the sub expression
    ev-define-2 ; second define label so it can assign the first argument to the result of evaluating second argument
        (pop (reg continue)) ; getting continue back
        (pop (reg exp))
        (assign (reg temp-var) CADR (reg exp)) ; getting the to-define part of (define to-define sub-expr)
        (perform (op define-var!) (reg temp-var) (reg val) (reg env)) ; defining the variable with the result of the evaluated sub expression
        (goto (reg continue)) ; finally goes to continue

    ev-apply ; expression goes to here when it is not a special form (self-evaluating, variable, define)
        (push (reg exp))
        (push (reg continue))
        (assign (reg exp) CAR (reg exp))
        (assign (reg continue) (label ev-apply-2))
        (goto (label eval))
    ev-apply-2
        (pop (reg continue))
        (pop (reg exp))
        (assign (reg proc) (reg val)) ; assignined proc to result of evaluating CAR (reg exp)
        (assign (reg unev) CDR (reg exp)) ; setting arguments up for recursive evalutating
        (assign (reg argl) CONS (reg null) (reg null))
        (push (reg continue)) ; saving continue
        (goto (label ev-apply-3))
    ev-apply-3
        (test (op null?) (reg unev)) ; if (reg unev) is null, done evaluating arguments so we can go to apply
        (branch (label apply))
        (push (reg argl)) ; pushing argl 
        (assign (reg continue) (label ev-apply-3-rec)) ; after evaluating the first element of unev, want to go to ev-apply-3-rec
        (assign (reg exp) CAR (reg unev))
        (goto (label eval))
    ev-apply-3-rec
        ;(goto (label done))
        (assign (reg val) CONS (reg val) (reg null)) ; making (reg val) into a one element list
        (pop (reg argl)) ; getting argl from the stack
        (assign (reg temp-1) (reg argl)) ; appending val to argl
        (assign (reg temp-2) (reg val))
        (assign (reg continue) (label ev-apply-3-rec-2))
        (goto (label append))
    ev-apply-3-rec-2
        (assign (reg argl) (reg temp-val))
        ((op display) (const "\n "))
        ((op display) (reg unev))
        (assign (reg unev) CDR (reg unev))
        (goto (label ev-apply-3))

    apply
        (pop (reg continue))
        (perform (op display) (reg argl))
        (assign (reg temp-1) CADDR (reg argl))
        (assign (reg temp-2) CADR (reg argl))
        (assign (reg val) (op +) (reg temp-1) (reg temp-2))
        (goto (reg continue))
        

    append ; inputs: temp-1, temp-2
        (push (reg continue))
        (push (reg temp-1)) ; saving the value of temp-1
        (assign (reg temp-val) CDR (reg temp-1))
        (test (op null?) (reg temp-val))
        (branch (label append-post))
        (assign (reg continue) (reg append-rec))
        (assign (reg temp-1) CDR (reg temp-1))
        (goto (label append))

    append-post
        (perform (op vector-set!) (reg the-cdrs) (reg temp-1) (reg temp-2))

    append-rec
        (pop (reg temp-val))
        (perform (op display) (reg temp-val))
        (pop (reg continue))
        (goto (reg continue))

    ;append-rec
    ;    (pop (reg temp-val)) ; getting what has been pushed by append
    ;    (pop (reg continue))
    ;    (perform (op display) (reg temp-val))
    ;    ;(perform (op display) (reg continue))
    ;    (goto (reg continue))
        


    cons  ; inputs: p-1, p-2
        (perform (op vector-set!) (reg the-cars) (reg free) (reg p-1))
        (perform (op vector-set!) (reg the-cdrs) (reg free) (reg p-2))
        (assign (reg p-val) (reg free)) ; assignes p-val to free, allows user to get the pointer that points to the cons pair
        (assign (reg free) ((op increment-pointer) (reg free))) ; increments free
        (goto (label p-cont))

    car  ; input: p-1
        (assign (reg p-val) (perform (op vector-ref) (reg the-cars) (reg p-1)))
        (perform (op vector-ref) (reg the-cars) (reg p-1))
        (goto (reg p-cont))

    cdr ; input: p-1
        (assign (reg p-val) (perform (op vector-ref) (reg the-cdrs) (reg p-1)))
        (perform (op vector-ref) (reg the-cdrs) (reg p-1))
        (goto (reg p-cont))

    done

    ))
