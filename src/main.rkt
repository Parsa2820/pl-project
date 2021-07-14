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

  (run "a=[4,3,True];print(a,4); for i in a: b = i; print(b); ; print(b); pass; a = True * False; print(a);")
  (displayln "")
  (run "e1=[1,2];print(e1);")
  )             
