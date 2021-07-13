(module parser racket

  (require parser-tools/lex
           (prefix-in : parser-tools/lex-sre)
           parser-tools/yacc)

  (require "datatype.rkt")

  (define-lex-abbrevs
    (special-keyword (:or "and"       "for"       "return"   
                          "break"     "else"      "global"    "not"     
                          "if"        "or"          
                          "continue"   "pass"       
                          "def"      "in"        "print"))
    )

  (define python-lexer
    (lexer
     (special-keyword  (string->symbol lexeme))
     ((:: (:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) ) (token-NUM (string->number lexeme)))
     ("True" (token-BOOLEAN #t))
     ("False" (token-BOOLEAN #f))
     ("NONE" (token-NONE))
     ((:: (:& (:+ (char-range #\a #\z)) (:& (complement "True") (complement "not") (complement "False"))) ) (token-ID (string->symbol lexeme)) )
     ("==" (string->symbol lexeme))
     (">" (string->symbol lexeme))
     ("<" (string->symbol lexeme))
     ("and" (string->symbol lexeme))
     ("or" (string->symbol lexeme))
     ("not" (string->symbol lexeme))
     ("(" (token-|(|))
     (")" (token-|)|))
     ("[" (token-|[|))
     ("]" (token-|]|))
     ("+" (token-+))
     (";" (token-TERMINATE))
     (whitespace (python-lexer input-port))
     ((eof) (token-EOF))
     )
    )

  (define-tokens a (NUM BOOLEAN ID))
  
  (define-empty-tokens et
    (and      for  NONE TERMINATE  EOF  return  == > < ** |(| |)| |[| |]| + - 
              break     else      global  not     
              if        or          
              continue   pass       
              def      in        print))

  (define lexSubString
    (lambda (subString listOfTokens)
      (if (null? subString)
          listOfTokens
          (begin (display  (car subString)) (cons (python-lexer (open-input-string (car subString))) (lexSubString (cdr subString) listOfTokens ))))))

  (define lex-this (lambda (lexer input) (lambda () (lexer input))))
  (define my-lexer (lex-this python-lexer (open-input-string "12; koskesh and krii; kiri == salam < kooni; True; 3; [salam aziam 45 63 True]; salam[2]; salam(6 3 kho); 10+238; kos+kir; salam(); salam; kir4; +5; 13; koskir; kos/kir;return 4;")))


  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  (my-lexer)
  #|
  (define python-parser
    (parser
     (start st)
     (end EOF)
     (error void)
     (tokens a et)
     (grammar (st
               ((return exp TERMINATE) (list $2))
               ;()
               ))
     (grammar (exp
               ((NUM) ($1))
               ))
     )
    )

  (define py-lexer (lex-this python-lexer (open-input-string "return 4;")))
  (let ((parser-res (python-parser py-lexer))) parser-res)
|#
  )