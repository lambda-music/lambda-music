
(define (defined? k) 
  (environment-bound? (interaction-environment) k ))

(define (define-if-not-exists k v)
  (if (defined? k)
      #f
      (eval (list 'define k (list 'quote v)))))

(define-macro define-if-not-defined
  (lambda (k v)
    `(define-if-not-exists ',k ,v)))
