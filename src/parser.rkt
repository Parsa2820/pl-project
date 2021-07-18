(module parser racket

  (require parser-tools/lex (prefix-in : parser-tools/lex-sre) parser-tools/yacc)
  (require "datatype.rkt")

  (provide (all-defined-out))

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
     ((:: (:or (:+ (char-range #\0 #\9))
               (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))))
      (token-NUM (string->number lexeme)))
     ("True" (token-BOOLEAN #t))
     ("False" (token-BOOLEAN #f))
     ("None" (token-NONE))
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
     ("," (token-|,|))
     (":" (token-:))
     ("+" (token-+))
     ("=" (token-=))
     ("-" (token--))
     ("*" (token-*))
     ("**" (token-**))
     ("/" (token-/))
     (";" (token-TERMINATE))
     (whitespace (python-lexer input-port))
     ((eof) (token-EOF))
     ((:: (:or (:or (:/ #\A #\Z) (:/ #\a #\z)) "_")
          (:* (:or (:or (:/ #\A #\Z) (:/ #\a #\z)) (:/ #\0 #\9) "_")))
      (token-ID (string->symbol lexeme)))
     )
    )

  (define-tokens a (NUM BOOLEAN ID))
  
  (define-empty-tokens et
    (and      for  NONE TERMINATE  EOF  return  == > < ** |(| |)| |[| |]| + - = |,| :
              break     else  *    global  not     
              if        or    /      
              continue   pass       
              def      in        print))

  (define lexSubString
    (lambda (subString listOfTokens)
      (if (null? subString)
          listOfTokens
          (begin (display (car subString))
                 (cons (python-lexer (open-input-string (car subString)))
                       (lexSubString (cdr subString) listOfTokens ))))))

  (define lex-this
    (lambda (lexer input)
      (lambda ()
        (lexer input)))
    )

  (define python-parser
    (parser
     (start program)
     (end EOF)
     (error void)
     (tokens a et)
     (grammar 
      (program
       ((sts) (program-base $1))
       )
      (sts
       ((st TERMINATE) (statements-base $1))
       ((sts st TERMINATE) (statements-multi $2 $1))
       )
      (st
       ((simple-st) (statement-simple-st $1))
       ((compound-st) (statement-compound-st $1))
       )
      (simple-st
       ((ID = exp) (assignment-st $1 $3))
       ((return-statement) (return-st $1))
       ((global ID) (global-st $2))
       ((pass) (pass-st))
       ((break) (break-st))
       ((continue) (continue-st))
       ((print |(| atoms |)|) (print-st $3))
       )
      (atoms
       ((atom) (list $1))
       ((atoms |,| atom) (append $1 (list $3)))
       )
      (return-statement
       ((return) (return-void))
       ((return exp) (return-exp $2))
       )
      (compound-st
       ((function) (function-def-st $1))
       ((if exp : sts else : sts) (if-st $2 $4 $7))
       ((for ID in exp : sts) (for-st $2 $4 $6))
       )
      (function
       ((def ID |(| params |)| : sts) (function-with-input $2 $4 $7))
       ((def ID |(| |)| : sts) (function-no-input $2 $6))
       )
      (params
       ((ID = exp) (params-base (param-with-defualt-base $1 $3)))
       ((params |,| ID = exp) (params-multi (param-with-defualt-base $3 $5) $1)) 
       )
      (exp
       ((disjunction) (expression-base $1))
       )
      (disjunction
       ((conjunction) (disjunction-base $1))
       ((disjunction or conjunction) (disjunction-or $3 $1))
       )
      (conjunction
       ((inversion) (conjunction-base $1))
       ((conjunction and inversion) (conjunction-and $1 $3))
       )
      (inversion
       ((not inversion) (inversion-not $2))
       ((comparison) (inversion-base $1))
       )
      (comparison
       ((sum cosps) (comparison-compare $1 $2))
       ((sum) (comparison-base $1))
       )
      (cosps
       ((cosp) (compare-op-sum-pairs-base $1))
       ((cosps cosp) (compare-op-sum-pairs-multi $2 $1)))
      (cosp
       ((== sum) (eq-sum $2))
       ((< sum) (lt-sum $2))
       ((> sum) (gt-sum $2))
       )
      (sum
       ((sum + term) (sum-add $1 $3))
       ((sum - term) (sum-subtract $1 $3))
       ((term) (sum-base $1))
       )
      (term
       ((term * factor) (term-multiplication $1 $3))
       ((term / factor) (term-division $1 $3))
       ((factor) (term-base $1))
       )
      (factor
       ((+ factor) (factor-affirmation $2))
       ((- factor) (factor-negation $2))
       ((power) (factor-base $1))
       )
      (power
       ((atom ** factor) (power-pow $1 $3))
       ((primary) (power-base $1))
       )
      (primary
       ((atom) (primary-base $1))
       ((primary |[| exp |]|) (primary-lst-index $1 $3))
       ((primary |(| |)|) (primary-call-function-no-args $1))
       ((primary |(| arguments |)|) (primary-call-function $1 $3))
       )
      (arguments
       ((exp) (arguments-base $1))
       ((arguments |,| exp) (arguments-multi $3 $1))
       )
      (atom
       ((ID) (atom-identifier $1))
       ((BOOLEAN) (atom-bool $1))
       ((NONE) (atom-none))
       ((NUM) (atom-number $1))
       ((lst) (atom-lst $1))
       )
      (lst
       ((|[| |]|) (empty-lst))
       ((|[| exps |]|) (not-empty-lst $2))
       )
      (exps
       ((exp) (expressions-base $1))
       ((exps |,| exp) (expressions-multi $3 $1))
       )
      )
     )
    )
  )
