#|
This module is some utilities for preventing lazy evaluation when side effect is possible.
It is not added to assign-st because it isn't needed in this project.
|#

(module lazy-utils racket

  (require (lib "eopl.ss" "eopl"))
  (require "datatype.rkt")
  
  (provide rhs-contains-function-call)

  (define (rhs-contains-function-call rhs)
    (if (expression? rhs)
        (expression-cfc rhs)
        #f)
    )

  (define (expression-cfc exp)
    (cases expression exp
      (expression-base (dis) (disjunction-cfc dis))
      )
    )

  (define (disjunction-cfc dis)
    (cases disjunction dis
      (disjunction-base (con) (conjunction-cfc con))
      (disjunction-or (con inner-dis) (or (conjunction-cfc con) (disjunction-cfc inner-dis)))
      )
    )

  (define (conjunction-cfc con)
    (cases conjunction con
      (conjunction-base (inv) (inversion-cfc inv))
      (conjunction-and (inner-con inv) (or (conjunction-cfc inner-con) (inversion-cfc inv)))
      )
    )

  (define (inversion-cfc inv)
    (cases inversion inv
      (inversion-not (inner-inv) (inversion-not inner-inv))
      (inversion-base (com) (comparison-cfc com)))
    )

  (define (comparison-cfc com)
    (cases comparison com
      (comparison-compare (s another-s) (or (sum-cfc s) (compare-op-sum-pairs-cfc another-s)))
      (comparison-base (s) (sum-cfc s)))
    )

  (define (sum-cfc s)
    (cases sum s
      (sum-add (inner-s t) (or (sum-cfc inner-s) (term-cfc t)))
      (sum-subtract (inner-s t) (or (sum-cfc inner-s) (term-cfc t)))
      (sum-base (t) (term-cfc t)))
    )

  (define (compare-op-sum-pairs-cfc cosps)
    (cases compare-op-sum-pairs cosps
      (compare-op-sum-pairs-base (cosp) (compare-op-sum-pair-cfc cosp))
      (compare-op-sum-pairs-multi (car-cosp cdr-cosp)
                                  (or
                                   (compare-op-sum-pair-cfc car-cosp)
                                   (compare-op-sum-pairs-cfc cdr-cosp))))
    )

  (define (compare-op-sum-pair-cfc cosp)
    (cases compare-op-sum-pair cosp
      (eq-sum (s) (sum-cfc s))
      (lt-sum (s) (sum-cfc s))
      (gt-sum (s) (sum-cfc s)))
    )

  (define (term-cfc t)
    (cases term t
      (term-multiplication (inner-t f) (or (term-cfc inner-t) (factor-cfc f)))
      (term-division (inner-t f) (or (term-cfc inner-t) (factor-cfc f)))      
      (term-base (f) (factor-cfc f)))
    )

  (define (factor-cfc f)
    (cases factor f
      (factor-affirmation (inner-f) (factor-cfc inner-f))
      (factor-negation (inner-f) (factor-cfc inner-f))
      (factor-base (p) (power-cfc p)))
    )

  (define (power-cfc p)
    (cases power p
      (power-pow (a f) (or (atom-cfc a) (factor-cfc f)))
      (power-base (pr) (primary-cfc pr)))
    )

  (define (atom-cfc a)
    (cases atom a
      (atom-lst (val) (lst-cfc val))
      (else #f))
    )

  (define (primary-cfc p)
    (cases primary p
      (primary-base (a) (atom-cfc a))
      (primary-lst-index (inner-p exp) (or (primary-cfc inner-p) (expression-cfc exp)))
      (primary-call-function-no-args (inner-p) #t)
      (primary-call-function (inner-p args) #t)
      )
    )

  (define (lst-cfc l)
    (cases lst l
      (empty-lst () #f)
      (not-empty-lst (exps) (expressions-cfc exps)))
    )

  (define (expressions-cfc exps)
    (cases expressions exps
      (expressions-base (exp) (expression-cfc exp))
      (expressions-multi (car-exp cdr-exp) (or (expression-cfc car-exp) (expressions-cfc cdr-exp))))
    )
  )
