; One way to implement this generalized apply-generic is to perform the operation on the first two arguments by raising appropriately, and then performing the operation on the result of that and the next argument, and repeat.
; One such case where the given algorithm does not work is when none of the tags of the arguments have direct implementations of the operation within the table, so apply generic will never be abl to look up the fucnctino properly.

; ('real-num 5) ('real-num 5), when attempting to add together will not find a function with the given algorithm
