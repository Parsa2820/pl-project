(module datatype racket

  (require (lib "eopl.ss" "eopl"))

  (provide (all-defined-out))

  (define-datatype program program?
    (program-base
     (sts statements?))
    )

  (define-datatype statements statements?
    (statements-base
     (st statement?))
    (statements-multi
     (car-st statement?)
     (cdr-st statements?))
    )

  (define-datatype statement statement?
    (statement-simple-st
     (st simple-st?))
    (statement-compound-st
     (st compound-st?))
    )

  (define-datatype simple-st simple-st?
    (assignment-st
     (lhs symbol?)
     (rhs expression?))
    (return-st
     (return-type return-datatype?))
    (global-st
     (id symbol?))
    (pass-st)
    (break-st)
    (continue-st)
    (print-st
     (vals (list-of atom?)))
    )

  (define-datatype compound-st compound-st?
    (function-def-st
     (func function-datatype?))
    (if-st
     (exp expression?)
     (sts-true statements?)
     (sts-false statements?))
    (for-st
     (id symbol?)
     (exp expression?)
     (sts statements?))
    )

  (define-datatype return-datatype return-datatype?
    (return-void)
    (return-exp
     (exp expression?))
    )

  (define-datatype function-datatype function-datatype?
    (function-no-input
     (name symbol?)
     (sts statements?))
    (function-with-input
     (name symbol?)
     (parameters params?)
     (sts statements?)
     )
    )

  (define-datatype params params?
    (params-base
     (param param-with-defualt?))
    (params-multi
     (car-param param-with-defualt?)
     (cdr-param params?))
    )
  
  (define-datatype param-with-defualt param-with-defualt?
    (param-with-defualt-base
     (id symbol?)
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
     (another-s compare-op-sum-pairs?))
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

  (define-datatype compare-op-sum-pairs compare-op-sum-pairs?
    (compare-op-sum-pairs-base
     (cosp compare-op-sum-pair?))
    (compare-op-sum-pairs-multi
     (car-cosp compare-op-sum-pair?)
     (cdr-cosp compare-op-sum-pairs?))
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
    (primary-call-function-no-args
     (p primary?))
    (primary-call-function
     (p primary?)
     (args arguments?))
    )

  (define-datatype arguments arguments?
    (arguments-base
     (exp expression?))
    (arguments-multi
     (car-arg expression?)
     (cdr-arg arguments?))
    )

  (define-datatype atom atom?
    (atom-identifier
     (id symbol?))
    (atom-bool
     (val boolean?))
    (atom-none)
    (atom-number
     (val number?))
    (atom-lst
     (val lst?))
    )

  (define-datatype lst lst?
    (empty-lst)
    (not-empty-lst
     (exps expressions?))
    )

  (define-datatype expressions expressions?
    (expressions-base
     (exp expression?))
    (expressions-multi
     (car-exp expression?)
     (cdr-exp expressions?))
    )
  )
