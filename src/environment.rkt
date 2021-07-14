
(module environment racket
  (require (lib "eopl.ss" "eopl"))
  (provide (all-defined-out))
  (define report-invalid-reference
    (lambda (ref the-store)
      (error "invalid refrence")))

  (define-datatype environment environment?
    [empty-env]
    [extended-env [bvar symbol?]
                [bval reference?]
                [saved-env environment?]])

  (define extend-env
    (lambda (var val saved-env)
      (extended-env var val saved-env)))

  (define apply-env
    (lambda (env search-sym)
      (cases environment env
        [extend-env (bvar bval saved-env) (if (eqv? search-sym bvar)
                                              bval
                                              (apply-env saved-env search-sym))])))
  )
    