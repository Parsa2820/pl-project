(module main racket
  (require "parser.rkt")
  (provide run)
  (define (run)
    (parse 'program)
    )
  (run)
  )