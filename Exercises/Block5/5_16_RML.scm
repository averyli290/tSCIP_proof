
(define (the-program) ; the lisp 2 program that will be loaded into the special machine
    '(
      ;(define x 13)
      ;(define y 12)
      ;(define x 6)
      ;(+ x (+ x y))
      ;(+ 1 1)
      ;((lambda (x y) (* (+ x 1) (+ y 1))) 3 3)
      ;(define f (lambda (x) (+ 1 x)))
      ;(f 7)
      ;(define x 11)
      ;(define f (lambda (x) (+ 1 x)))
      ;(f 7)
      ;x
      (define x 1)
      (if (= x 1) 16 17)
      ;(if (= 2 (if (= 1 2) 1 2)) 13 14)
      ;(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
      ;(fact 2)
      ))


(define (get-RML)
  '(
    (assign (reg env) (op make-environment))
    ;(assign (reg global-env) (reg env))
    ; allowing the program to return back to the global environment (is just a pointer, so it it updates along with env)
    ; after doing an application that creates new frame and sets (reg env) to that new frame

    ; setting primitives up
    (assign (reg temp-1) (reg null))
    (assign (reg temp-1) CONS (const +) (reg temp-1))
    (assign (reg temp-1) CONS (const primitive) (reg temp-1))
    (perform (op define-var!) (const +) (reg temp-1) (reg env))

    (assign (reg temp-1) (reg null))
    (assign (reg temp-1) CONS (const *) (reg temp-1))
    (assign (reg temp-1) CONS (const primitive) (reg temp-1))
    (perform (op define-var!) (const *) (reg temp-1) (reg env))

    (assign (reg temp-1) (reg null))
    (assign (reg temp-1) CONS (const -) (reg temp-1))
    (assign (reg temp-1) CONS (const primitive) (reg temp-1))
    (perform (op define-var!) (const -) (reg temp-1) (reg env))

    (assign (reg temp-1) (reg null))
    (assign (reg temp-1) CONS (const /) (reg temp-1))
    (assign (reg temp-1) CONS (const primitive) (reg temp-1))
    (perform (op define-var!) (const /) (reg temp-1) (reg env))

    (assign (reg temp-1) (reg null))
    (assign (reg temp-1) CONS (const =) (reg temp-1))
    (assign (reg temp-1) CONS (const primitive) (reg temp-1))
    (perform (op define-var!) (const =) (reg temp-1) (reg env))


    eval-loop
        ;#################################################################
        ;# TRY CHANGING GLOBAL-ENV IDEA TO PUSHING AND POPPING ENVIRONMENT
        ;#################################################################

        ;(assign (reg env) (reg global-env))
        
        (test (op null?) (reg program)) ; tests if program is null
        (branch (label done)) ; if so, goes to label done
        (assign (reg continue) (label eval-loop))
        (assign (reg exp) CAR (reg program))
        (assign (reg program) CDR (reg program))
        (goto (label eval))

    eval ; testing to see what to use to evaluate exp (expression)
        
        (push (reg env))

        (test (op number?) (reg exp))
        (branch (label ev-self-evalutating))
        (test (op symbol?) (reg exp))
        (branch (label ev-variable))
        (assign (reg temp-var) CAR (reg exp))
        (test (op eq?) (const define) (reg temp-var))
        (branch (label ev-define))

        (assign (reg temp-1) CAR (reg exp))
        (test (op eq?) (const lambda) (reg temp-1)) ; testing for lambda
        (branch (label ev-lambda))
        (test (op eq?) (const if) (reg temp-1)) ; testing for if statement
        (branch (label ev-if))

        (goto (label ev-apply)) ; otherwise go to apply because it is not recognized


    ev-self-evalutating
        (assign (reg val) (reg exp))
        (pop (reg env))
        (goto (reg continue)) ; goes to the label stored in continue after any evaluation

    ev-variable
        (assign (reg val) (perform (op lookup-var-value) (reg exp) (reg env))) ; looking up the value of the variable in the environment
        (pop (reg env))
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
        (pop (reg env))
        (perform (op define-var!) (reg temp-var) (reg val) (reg env)) ; defining the variable with the result of the evaluated sub expression
        (goto (reg continue)) ; finally goes to continue

    ev-lambda ; if the expression is like (lambda (vars . ) (body .))
        (assign (reg val) (reg null)) ; returns in (reg val) as (procedure (vars. ) (body . ) env)
        (assign (reg val) CONS (reg env) (reg val))
        (assign (reg temp-1) CDDR (reg exp)) ; getting the body (is a LIST, so CDDR, not CADDR)
        (assign (reg val) CONS (reg temp-1) (reg val))
        (assign (reg temp-1) CADR (reg exp))
        (assign (reg val) CONS (reg temp-1) (reg val))
        (assign (reg val) CONS (const procedure) (reg val))
        (pop (reg env))
        (goto (reg continue)) ; goes to continue after it is done

    ev-if ; if the expression is exp (if (...) ... ...)
        ;(pop (reg env))
        (push (reg continue)) ; saving continue and exp before of recursive call
        (push (reg exp))
        (push (reg env))
        (assign (reg continue) (label ev-if-2))
        (assign (reg exp) CADR (reg exp))
        (goto (reg eval))
    ev-if-2
        (pop (reg env))
        (test (op =) (reg val) (const 1)) ; testing if the if statement evaluated to true
        (branch (label ev-if-true)) ; goes to ev-if-true if val==1
        (goto (label ev-if-false)) ; goes to ev-if-false otherwise
    ev-if-true
        ;(pop (reg env))
        (pop (reg exp)) ; getting the original expression back (if (...) ... ...)
        (pop (reg continue))
        (assign (reg exp) CADDR (reg exp))
        (goto (reg eval)) ; don't need to save continue because this is the continue that you want to go to after the if statement is evaluated in full
    ev-if-false
        ;(pop (reg env))
        (pop (reg exp)) ; getting the original expression back (if (...) ... ...)
        (pop (reg continue))
        (assign (reg exp) CADDDR (reg exp))
        (goto (reg eval)) ; don't need to save continue because this is the continue that you want to go to after the if statement is evaluated in full

    ev-apply ; expression goes to here when it is not a special form (self;-evaluating, variable, define)
        (push (reg exp)) ; pushing exp and continue to the stack 
        (push (reg continue)) ; NEED to push things that are being used to stack when doing either recursive calls or subroutines to be safe
        (assign (reg exp) CAR (reg exp)) ; evaluating the first argument of (reg exp) and putting it in (reg proc)
        (assign (reg continue) (label ev-apply-2)) ; makes the function go to ev-apply-2 after evaluation
        (goto (label eval))
    ev-apply-2 ; the next stage of apply starts with the evaluated procedure in (reg proc) and setting up for the evaluating the arguments
        (pop (reg continue)) ; getting the values of continue and reg back
        (pop (reg exp))
        (assign (reg proc) (reg val)) ; assignined proc to result of evaluating CAR (reg exp)
        (assign (reg unev) CDR (reg exp)) ; setting arguments up for recursive evalutating
        (assign (reg argl) (reg null)) ; assigning (reg argl) to '() at first
        (push (reg continue)) ; saving continue because ev-apply-3, ev-apply-3-rec
        (goto (label ev-apply-3-rec))
    ev-apply-3-rec
        (test (op null?) (reg unev)) ; if (reg unev) is null, done evaluating arguments so we can go to apply
        (branch (label apply))
        (push (reg continue))
        (push (reg proc)) ; pushing proc to the stack because it may be overwritten when evaluating arguments such as (* (+ 1 1) 2), * overwritten by +
        (push (reg argl)) ; pushing argl and unev to save them
        (push (reg unev)) ; have to push (reg unev) to the stack because (reg unev) might be overidden in recursion, like in the case of (+ 1 (+ 2 3)) (the inside part overrides the outside part)
        (assign (reg continue) (label ev-apply-3-rec-2)) ; after evaluating the first element of unev, want to go to ev-apply-3-rec
        (assign (reg exp) CAR (reg unev))
        (goto (label eval))
    ev-apply-3-rec-2
        (assign (reg val) CONS (reg val) (reg null)) ; making (reg val) into a one element list
        (pop (reg unev)) ; getting the saved value of unev
        (pop (reg argl)) ; getting argl from the stack
        (pop (reg proc)) ; getting proc from the stack
        (pop (reg continue))
        (assign (reg temp-1) (reg argl)) ; appending val to argl
        (assign (reg temp-2) (reg val))
        (push (reg continue)) ; saving the continue register because the register is needed for a recursive call
        (assign (reg continue) (label ev-apply-3-rec-3))
        (goto (label append))
    ev-apply-3-rec-3
        (pop (reg continue)) ; getting continue register back
        (assign (reg argl) (reg temp-val)) ; assigning the (reg argl) argument list to be the result of append
        (assign (reg unev) CDR (reg unev)) ; continuing on in unev
        (goto (label ev-apply-3-rec)) ; going back to the start of evaluating (reg unev)

    apply
        (assign (reg temp-1) CAR (reg proc))
        (test (op eq?) (reg temp-1) (const primitive)) ; checking if the procedure is primitive
        (branch (label apply-primitive)) ; goes to apply-primitive if primitive
        ; otherwise assumes that (reg proc) is a 4-tuple representing procedure
        (assign (reg env) CADDDR (reg proc)) ; getting the environment to extend for running the procedure in
        (assign (reg temp-1) CADR (reg proc))
        (assign (reg env) (op extend-environment) (reg temp-1) (reg argl) (reg env)) ; extending the environment using the variables from procedure, the evaluated arguments, and environment
        (assign (reg exp) CADDR (reg proc))
        (assign (reg exp) CAR (reg exp)) ; for the purposes of this exercise, assumes that the body of the procedure is only 1 element long, but it can be more
        (goto (label eval)) ; evaluates the body of the procedure


    apply-primitive ; goes to this section when a procedure is primitive
        (pop (reg continue)) ; getting continue from the stack
        (assign (reg temp-1) CADR (reg proc)) ; get the primitive name from the pair
        (assign (reg val) (op primitive-apply) (reg temp-1) (reg argl)) ; using the level-0 lisp here
        (pop (reg env))
        (goto (reg continue))
        
    append ; inputs: temp-1, temp-2
        (push (reg continue))
        (test (op null?) (reg temp-1))
        (branch (label append-post))
        (assign (reg temp-var) CAR (reg temp-1))
        (push (reg temp-var))
        (assign (reg continue) (reg append-rec))
        (assign (reg temp-1) CDR (reg temp-1))
        (goto (label append))

    append-post
        (assign (reg temp-val) (reg temp-2))
        (pop (reg continue))
        (goto (reg continue))

    append-rec
        (pop (reg temp-var))
        (pop (reg continue))
        (assign (reg temp-val) CONS (reg temp-var) (reg temp-val))
        (goto (reg continue))


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
