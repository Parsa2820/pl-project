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

  (run "a=[1,2,3,4]; for i in a: if i == 2: print(i); break; else: print(23);; print(999); ; for i in a: print(i);;")
  )                           
