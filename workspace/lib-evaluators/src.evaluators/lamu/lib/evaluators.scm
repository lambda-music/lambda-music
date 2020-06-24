
(define (defined?-proc k) 
  (environment-bound? (interaction-environment) k ))

(define (defined-if-not-defined-proc k v)
  (if (defined?-proc k)
      #f
      (eval (list 'define k (list 'quote v)))))

(define-macro defined?
  (lambda (k)
    `(defined?-proc ',k)))

(define-macro define-if-not-defined
  (lambda (k v)
    `(defined-if-not-defined-proc ',k ,v)))
