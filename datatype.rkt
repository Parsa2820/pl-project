(module datatype racket

  (require (lib "eopl.ss" "eopl"))

  (define-datatype statement statement?
    (simple
     (st simple-statement?))
    (compound
     (st compound-statement?))
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
    (print
     (vals (list-of-not-null atom?)))
    )

  (define-datatype compound-statement compound-statement?
    (function-def-st
     (name identifier?)
     (params (list-of param-datatype?))
     (statements (list-of-not-null statement?)))
    (if-st
     (exp expression?)
     (statements-true (list-of-not-null statement?))
     (statements-false (list-of-not-null statement?)))
    (for-st
     (id identifier?)
     (exp expression?)
     (statements (list-of-not-null statement?)))
    )

  (define-datatype return-datatype return-datatype?
    (return-void)
    (return-exp
     (exp expression?))
    )

  (define-datatype param-datatype param-datatype?
    (param-with-defualt
     (id identifier?)
     (exp expression?))
    )

  (define (expression? e)
    (disjunction? e)
    )

  (define-datatype disjunction disjunction?
    (base
     (con conjunction?))
    (or
     (con conjunction?)
     (dis disjunction?))
    )

  (define-datatype conjunction conjunction?
    (base
     (inv inversion?))
    (and
     (con conjunction?)
     (inv inversion?))
    )

  (define-datatype inversion inversion?
    (not
     (inv inversion?))
    (base
     (com comparison?))
    )

  (define-datatype comparison comparison?
    (compare
     (s sum?)
     (another-s (list-of-not-null compare-op-sum-pair?)))
    (base
     (s sum?))
    )

  (define-datatype sum sum?
    (add
     (s sum?)
     (t term?))
    (subtract
     (s sum?)
     (t term?))
    (base
     (t term?))
    )

  (define-datatype compare-op-sum-pair compare-op-sum-pair?
    (eq-sum
     (s sum?))
    (lt-sum
     (s sum?))
    (gt-sum
     (s sum?))
    )

  (define-datatype term term?
    (multiplication
     (t term?)
     (f factor?))
    (division
     (t term?)
     (f factor?))
    (base
     (f factor?))
    )

  (define-datatype factor factor?
    (affirmation
     (f factor?))
    (negation
     (f factor?))
    (base
     (p power?))
    )

  (define-datatype power power?
    (pow
     (a atom?)
     (f factor?))
    (base
     (p primary?))
    )

  (define-datatype primary primary?
    (base
     (a atom?))
    (lst-index
     (p primary?)
     (exp expression?))
    (call-function
     (p primary?)
     (args (list-of expression?)))
    )

  (define-datatype atom atom?
    (identifier
     (id identifier?))
    (bool
     (val boolean?))
    (none)
    (number
     (val number?))
    (lst
     (val (list-of expression?)))
    )
  
  (define (list-of-not-null pre)
    (lambda (lst)
      (if (null? lst)
          #f
          ((list-of pre) lst))
      )
    )
  
  )