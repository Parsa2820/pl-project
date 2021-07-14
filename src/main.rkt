(module main racket
  
  (require "datatype.rkt")
  (require "interpreter.rkt")
  (require "parser.rkt")

  (provide run)

  (define (run pgm-string)
    (define py-lexer (lex-this python-lexer (open-input-string pgm-string)))
    (let ((parser-res (python-parser py-lexer-2)))
      (value-of-program parser-res))
    )

  (run "print(3.14);")
  
  )                           
