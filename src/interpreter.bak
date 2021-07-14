(module interpreter racket
  
  (require (lib "eopl.ss" "eopl"))
  (require "datatype.rkt")
  (require "environment.rkt")
  (require "store.rkt")

  (provide (all-defined-out))
  
  (define global-env (empty-env))
  (define local-envs (list))

  (define value-of-program
    (lambda (prgm)
      (cases program prgm
        (program-base (sts) (value-of-stmts sts global-env))))
    )

  (define value-of-stmts
    (lambda (stmts env)
      ;(if (and (deref (apply-env 'break)) (derref (apply-env 'continue)))
          (cases statements stmts      
            (statements-base (st) (value-of-stmt st env))
            (statements-multi (car-st cdr-st) (begin (value-of-stmts cdr-st env) (value-of-stmt car-st env)))))
    )
       
  (define value-of-stmt
    (lambda (stmt env)
      (cases statement stmt
        (statement-simple-st (st) (value-of-simple-st st env))
        (statement-compound-st (st) (value-of-compound-st st env))))
    )

  (define value-of-simple-st
    (lambda (st env)
      (cases simple-st st
        (assignment-st (lhs rhs) (begin (set! global-env (extend-env lhs (newref (value-of-exp rhs global-env)) global-env)) (display global-env)))
        ;(return-st) ;(return-type) (value-of-return-type return-type))
        (global-st (id) (extend-env id (apply-env id global-env) env))
        (pass-st () (void))
        ;(break-st)
        ;(continue-st)
        (print-st (vals) (print-atoms-lst vals env))
        (else (void))))
    )

  (define (print-atoms-lst atoms-lst env)
    (cond
      [(null? atoms-lst) (void)]
      [else
       (begin
         (display (value-of-atom (car atoms-lst) env))
         (display " ")
         (print-atoms-lst (cdr atoms-lst) env))])
    )

  ;(define value-of-return
  

  (define value-of-compound-st
    (lambda (st env)
      (cases compound-st st
        (function-def-st (func) (value-of-fun func))
        (if-st (exp true-stmts false-stmts) (if (value-of-exp exp env) (value-of-stmts true-stmts env) (value-of-stmts false-stmts env)))
        (for-st (id exp stmts) (value-of-for id exp stmts env))))
    )

  (define value-of-for
    (lambda (id exp stmts env)
      (let ((expval (value-of-exp exp env)))
          (if (null?  expval)
              (void)
              (begin
                (value-of-for id (cdr expval) stmts env)
                (value-of-stmts stmts (extend-env id (car expval) env))))))
    )

  (define value-of-fun
    (lambda (func)
      (cases function-datatype func
        (function-no-input (id stmts env) (set! global-env (extend-env id (newref func) global-env)))
        (function-with-input (id params sts env) (set! global-env (extend-env id (newref func) global-env))))))
 
  (define value-of-exp
    (lambda (exp env)
      (cases expression exp
        (expression-base (dis) (value-of-dis dis env)))))

  (define value-of-dis
    (lambda (dis env)
      (cases disjunction dis
        (disjunction-base (con) (value-of-con con env))
        (disjunction-or (con dis) (or (value-of-dis dis env) (value-of-con con env))))))

  (define value-of-con
    (lambda (con env)
      (cases conjunction con
        (conjunction-base (inv) (value-of-inv inv env))
        (conjunction-and (con inv) (and (value-of-inv inv env) (value-of-con con env))))))

  (define value-of-inv
    (lambda (inv env)
      (cases inversion inv
        (inversion-base (comp) (value-of-comp comp env))
        (inversion-not (inv) (not (value-of-inv inv env))))))

  (define value-of-comp
    (lambda (comp env)
      (cases comparison comp
        (comparison-compare (s com-op-sum-pairs) (cases compare-op-sum-pairs com-op-sum-pairs
                                                   (compare-op-sum-pairs-base (cosp)
                                                                              (cases compare-op-sum-pair cosp
                                                                                (eq-sum (s2) (= (value-of-sum s env) (value-of-sum s2 env)))
                                                                                (lt-sum (s2) (< (value-of-sum s env) (value-of-sum s2 env)))
                                                                                (gt-sum (s2) (> (value-of-sum s env)  (value-of-sum s2 env)))))
                                                   (compare-op-sum-pairs-multi (car-cosp cdr-cosp) (cases compare-op-sum-pair car-cosp
                                                                                                     (eq-sum (s2) (=  (value-of-sum s env) (value-of-comp (comparison-compare s2 cdr-cosp) env)))
                                                                                                     (lt-sum (s2) (<  (value-of-sum s env) (value-of-comp (comparison-compare s2 cdr-cosp) env)))
                                                                                                     (gt-sum (s2) (>  (value-of-sum s env) (value-of-comp (comparison-compare s2 cdr-cosp) env)))))))
      (comparison-base (s) (value-of-sum s env)))))
  
#|
  (define value-of-comp
    (lambda (comp env)
      (cases comparison comp
        (comparison-compare (s cosps) ())
        (comparison-base (s) (value-of-sum s env)))))

  (define value-of-compare-op-sum-pairs
    (lambda (cosps env)
      (cases compare-op-sum-pairs-base cosps
        (compare-op-sum-pairs-base (cosp) (value-of-compare-op-sum-pair cosp env))
        (compare-op-sum-pairs-multi (car-cosp cdr-cosp) ())
        )
      )
    )
|#

  (define value-of-sum
    (lambda (s-dt env)
      (cases sum s-dt
        (sum-add (s t) (+ (value-of-sum s env) (value-of-term t env) ))
        (sum-subtract (s t) (- (value-of-sum s env) (value-of-term t env) ) )         
        (sum-base (t) (value-of-term t env)))))

  (define value-of-term
    (lambda (t env)
      (cases term t
        (term-multiplication (tt tf)  (* (value-of-term tt env) (value-of-factor tf env)))
        (term-division (tt tf)  (/ (value-of-term tt env) (value-of-factor tf env)))
        (term-base (tf)   (value-of-factor tf env)))))

  
  (define value-of-factor
    (λ (f env)
           (cases factor f
             (factor-affirmation (ff) (value-of-factor ff env))
             (factor-negation (ff) (- 0 (value-of-factor ff env)))
             (factor-base (fp) (value-of-power fp env)))))

  (define value-of-power
    (lambda (p env)
      (cases power p
        (power-pow (pa pf) (expt (value-of-atom pa env) (value-of-factor pf env)))
        (power-base (pprimary) (value-of-primary pprimary env)))))


  (define up-env-params
    (lambda (proc-params env)
      (cases params proc-params
        (params-base (param) (up-env-param-with-default param env))
        (params-multi (car-param cdr-param)
                      (up-env-params cdr-param (up-env-param-with-default car-param)))))
    )

  (define up-env-param-with-default
    (lambda (param env)
      (cases param-with-defualt param
        (param-with-defualt-base (id exp) (extend-env id (newref (value-of-exp exp env)) env))))
    )
  #|
  (define up-env-params-exps
    (lambda (proc-params exps env)
      (cases expression exps
        (expression-base (exp) (cases params proc-params)

                        ))))
    |#  
                                         
          
  (define call-no-input
    (lambda (function env)
      (cases function-datatype function
        (function-no-input (fun-name  fun-stmts saved-env)  (value-of-stmts fun-stmts saved-env))
        (else (void))))
    ) 
      

  (define value-of-primary
    (lambda (pri env)
      (cases primary pri
        (primary-base (pa) (value-of-atom pa env))
        (primary-lst-index (pp pe) (list-ref (value-of-primary pp env) (value-of-exp pe env)))
        (primary-call-function-no-args (pp) (call-no-input (value-of-primary pp env)  env))
        ;(primary-call-function (pp pa))
        (else (void))
        )
      )
    )

  (define value-of-atom
    (lambda (_atom env)
      (cases atom _atom
        (atom-identifier (id) (begin (display env) (deref (apply-env id global-env))))
        (atom-bool (val) val)
        (atom-none () null)
        (atom-number (val) val)
        (atom-lst (val) (value-of-lst val env))))
    )

  (define value-of-lst
    (lambda (_lst env)
      (cases lst _lst
        (empty-lst () '())
        (not-empty-lst (exps) (value-of-exps exps env))))
    )

  (define value-of-exps
    (lambda (_exps env)
      (cases expressions _exps
        (expressions-base (exp) (list (value-of-exp exp env)))
        (expressions-multi (car-exp cdr-exp)
                           (append (value-of-exps cdr-exp env) (list (value-of-exp car-exp env))))))
    )
)
