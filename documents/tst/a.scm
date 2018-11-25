
(define ls (list 0 1 2 3 4 5) )

(display 
  (let loop ((e ls)) 
    (if (null? e)
      '()
      (append (loop (cdr e))
              (list (car e))))))
(newline)

(display 
  (let loop ((e  ls )
             (a '() ))
    (if (null? e)
      a
      (loop (cdr e) 
            (cons (car e) a )))))
(newline)

; vim: expandtab:
