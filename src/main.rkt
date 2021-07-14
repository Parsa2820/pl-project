(module main racket
  
  (require "datatype.rkt")
  (require "interpreter.rkt")
  (require "parser.rkt")

  (provide run)

  (define (run pgm-string)
    (define py-lexer (lex-this python-lexer (open-input-string pgm-string)))
    (let ((parser-res (python-parser py-lexer)))
      (value-of-program parser-res))
    )

  (run "print(4);")
  )                           
