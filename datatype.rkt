(module datatype racket

  (require (lib "eopl.ss" "eopl"))

  (define-datatype statement statement?
    (simple
     (num number?))
    )

  (define-datatype simple-statement simple-statement?
    (assignment-st
     (lhs identifier?)
     (rhs expression?))
    (return-st
     (return-type return-datatype?))
    (global-st
     (id identifier?))
    (pass-st)
    (break-st)
    (continue-st)
    )

  (define-datatype simple-statement simple-statement?

    )

  (define-datatype return-datatype return-datatype?
    (return-void)
    (return-exp
     (exp expression?))
    )

  
  )