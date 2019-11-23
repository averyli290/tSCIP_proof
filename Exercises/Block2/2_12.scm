; elt-of-set runs in O(n) time, it needs to check at maximum n elements in the list
; add-to-set runs in O(n) time, it needs to check a maximum n elements in the list to make sure there will not be a repeat
; union takes O(n^2) time, it needs to check at maximum n elements of set-2 for each of the n elements of set-1 to make sure there are no repeats
; intersection takes O(n^2) time, for the same reason as union, but to make sure that they exist in both sets


