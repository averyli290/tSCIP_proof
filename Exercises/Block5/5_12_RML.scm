
(define (the-program) ; the lisp 2 program that will be loaded into the special machine
    '(
      (define x 13)
      (define y 12)
      x
      ))


(define (get-RML)
  '(
    (assign (reg env) (op make-environment))

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

        (assign (reg temp-1) CADR (reg exp)) ; TEMPORARY for evaluating ((define x (+ 1 3) '()) x '())
        (assign (reg temp-2) CADDR (reg exp))
        (assign (reg val) (op +) (reg temp-1) (reg temp-2))
        (goto (reg continue))

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
