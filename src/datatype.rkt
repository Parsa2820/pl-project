(module datatype racket

  (require (lib "eopl.ss" "eopl"))

  (provide (all-defined-out))

  (define-datatype statement statement?
    (statement-simple-st
     (st simple-st?))
    (statement-compound-st
     (st compound-st?))
    )

  (define-datatype simple-st simple-st?
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
    (print-st
     (vals (list-of-not-null atom?)))
    )

  (define-datatype compound-st compound-st?
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

  (define-datatype expression expression?
    (expression-base
     (dis disjunction?))
    )

  (define-datatype disjunction disjunction?
    (disjunction-base
     (con conjunction?))
    (disjunction-or
     (con conjunction?)
     (dis disjunction?))
    )

  (define-datatype conjunction conjunction?
    (conjunction-base
     (inv inversion?))
    (conjunction-and
     (con conjunction?)
     (inv inversion?))
    )

  (define-datatype inversion inversion?
    (inversion-not
     (inv inversion?))
    (inversion-base
     (com comparison?))
    )

  (define-datatype comparison comparison?
    (comparison-compare
     (s sum?)
     (another-s (list-of-not-null compare-op-sum-pair?)))
    (comparison-base
     (s sum?))
    )

  (define-datatype sum sum?
    (sum-add
     (s sum?)
     (t term?))
    (sum-subtract
     (s sum?)
     (t term?))
    (sum-base
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
    (term-multiplication
     (t term?)
     (f factor?))
    (term-division
     (t term?)
     (f factor?))
    (term-base
     (f factor?))
    )

  (define-datatype factor factor?
    (factor-affirmation
     (f factor?))
    (factor-negation
     (f factor?))
    (factor-base
     (p power?))
    )

  (define-datatype power power?
    (power-pow
     (a atom?)
     (f factor?))
    (power-base
     (p primary?))
    )

  (define-datatype primary primary?
    (primary-base
     (a atom?))
    (primary-lst-index
     (p primary?)
     (exp expression?))
    (primary-call-function
     (p primary?)
     (args (list-of expression?)))
    )

  (define-datatype atom atom?
    (atom-identifier
     (id identifier?))
    (atom-bool
     (val boolean?))
    (atom-none)
    (atom-number
     (val number?))
    (atom-lst
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