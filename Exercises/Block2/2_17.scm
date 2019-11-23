(define (make-node x y z) (list x y z))

(define (list->tree elements)
    (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
    (if (= n 0)
        (cons '() elts) ; Base Case: adds an empty list when there are no further elements to be added in current partial tree
            (let ((left-size (quotient (- n 1) 2))) ; Gets the amount of elements to be placed in the left partial tree of the current partial tree
                (let ((left-result (partial-tree elts left-size))) ; Creates left-result, which is made up of a left partial tree as the first element and the remaining elemnts to place in the right partial tree of the current partial tree
                    (let ((left-tree (car left-result)) ; Creates variables for the left partial tree of the current partial tree, the remaining elements to be put in the right partial tree, and the number of elements to be put in the right partial tree, however, we add one to the left-size because the middle number is going to be the entry in our current node
                          (non-left-elts (cdr left-result))
                          (right-size (- n (+ left-size 1))))
                        (let ((this-entry (car non-left-elts)) ; Assigns a variable to contain the value that the current node will have and also creates right-result containing a list which is the partial tree of the remaining elements that aren't in the left partial tree or is the current node value
                              (right-result (partial-tree (cdr non-left-elts)
                                                                right-size)))
                           (let ((right-tree (car right-result)) ; Assings right-tree to be the car of right-result, because the first element of right-result is the right-tree and the remaining elements is a list of the rest of the elements to add
                                 (remaining-elts (cdr right-result)))
                              (cons (make-node this-entry left-tree right-tree)
                                 remaining-elts)))))))) ; We create the partial tree by first creating the node and then returning other elements to next iteration



;(list->tree '(1 2 3 4 5 6))
