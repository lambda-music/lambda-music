(import (srfi 1))

(display
  (fold (lambda (f a) (apply f a ))
        (list 1 2 3) 
        (list (lambda( a b c ) (list a b c) )
              (lambda( a b c ) (list b ))
              (lambda(x) (cons 3 x))  )))
(newline)

; vim: expandtab :
