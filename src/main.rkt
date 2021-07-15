(module main racket
  
  (require "interpreter.rkt")

  (define (main pgm-string)
    (run pgm-string)
    )

  (define (main-file pgm-file-path)
    (evaluate-file pgm-file-path)
    )
  )            
