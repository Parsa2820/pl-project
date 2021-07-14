(module main racket
  
  (require "datatype.rkt")
  (require "interpreter.rkt")
  (require "parser.rkt")
  (require "store.rkt")

  (provide run)

  (define (run pgm-string)
    (define py-lexer (lex-this python-lexer (open-input-string pgm-string)))
    (begin (initialize-store!)
           (let ((parser-res (python-parser py-lexer)))
              (value-of-program parser-res))
              ;parser-res)
           )
    )

  (run "a=[4,3,True,    5, 6+1];print(1, 3, a);")
  )                           
