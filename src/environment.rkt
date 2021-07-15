(module environment racket
  
  (require (lib "eopl.ss" "eopl"))
  (require "store.rkt")
  
  (provide (all-defined-out))

  (define-datatype environment environment?
    [empty-env]
    [extended-env [bvar symbol?]
                [bval reference?]
                [saved-env environment?]]
    )

  (define extend-env
    (lambda (var val saved-env)
      (extended-env var val saved-env))
    )

  (define apply-env
    (lambda (search-sym env)
      (cases environment env
        [empty-env () null]
        [extended-env (bvar bval saved-env)
                      (if (eqv? search-sym bvar)
                          bval
                          (apply-env search-sym saved-env))]))
    )
  )
