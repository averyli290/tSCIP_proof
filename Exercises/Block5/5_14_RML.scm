
(define (the-program) ; the lisp 2 program that will be loaded into the special machine
    '(
      (define x 13)
      (define y 12)
      ;(define x 6)
      ;(+ x (+ x y))
      (+ 1 2 3)
      ;x 
      ))


(define (get-RML)
  '(
    (assign (reg env) (op make-environment))

    ; setting primitives up
    (assign (reg temp-1) (reg null))
    (assign (reg temp-1) CONS (const +) (reg temp-1))
    (assign (reg temp-1) CONS (const primitive) (reg temp-1))
    (perform (op define-var!) (const +) (reg temp-1) (reg env))

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
        (push (reg exp)) ; pushing exp and continue to the stack 
        (push (reg continue)) ; NEED to push things that are being used to stack when doing either recursive calls or subroutines to be safe
        (assign (reg exp) CAR (reg exp)) ; evaluating the first argument of (reg exp) and putting it in (reg proc)
        (assign (reg continue) (label ev-apply-2))
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
        (push (reg argl)) ; pushing argl and unev to save them
        (push (reg unev)) ; have to push (reg unev) to the stack because (reg unev) might be overidden in recursion, like in the case of (+ 1 (+ 2 3)) (the inside part overrides the outside part)
        (assign (reg continue) (label ev-apply-3-rec-2)) ; after evaluating the first element of unev, want to go to ev-apply-3-rec
        (assign (reg exp) CAR (reg unev))
        (goto (label eval))
    ev-apply-3-rec-2
        (assign (reg val) CONS (reg val) (reg null)) ; making (reg val) into a one element list
        (pop (reg unev)) ; getting the saved value of unev
        (pop (reg argl)) ; getting argl from the stack
        (assign (reg temp-1) (reg argl)) ; appending val to argl
        (assign (reg temp-2) (reg val))
        (assign (reg continue) (label ev-apply-3-rec-3))
        (goto (label append))
    ev-apply-3-rec-3
        (assign (reg argl) (reg temp-val)) ; assigning the (reg argl) argument list to be the result of append
        (assign (reg unev) CDR (reg unev)) ; continuing on in unev
        (goto (label ev-apply-3-rec)) ; going back to the start of evaluating (reg unev)

    apply
        ; for this exercise, apply assumes that the procedure is primitive
        (pop (reg continue)) ; getting continue from the stack
        (assign (reg temp-1) CADR (reg proc)) ; get the primitive name from the pair
        (assign (reg val) (op primitive-apply) (reg temp-1) (reg argl)) ; using the level-0 lisp here
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
