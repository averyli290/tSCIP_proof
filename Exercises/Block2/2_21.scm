(define (memq? sym li)
    (cond ((null? li) #f)
          ((eq? sym (car li)) #t)
          (else (memq? sym (cdr li)))))


