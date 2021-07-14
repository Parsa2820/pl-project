
(module main racket
  (include "datatype.rkt")
  (require (lib "eopl.ss" "eopl"))
  

  (define value-of-stmts
    (lambda (stmts env)
      (cases statements stmts
        (statements-base (st) (value-of-stmt st env))
        (statements-multi (car-st cdr-st) (begin (value-of-stmt car-st env) (value-of-stmts cdr-stenv))))))
           
  

 
  (define value-of-exp
    (lambda (exp env)
      (cases expression exp
        (expression-base (dis) (value-of-dis dis env)))))

  (define value-of-dis
    (lambda (dis env)
      (cases disjunction dis
        (disjunction-base (con) (value-of-con con env))
        (disjuction-or (con dis) (or (value-of-dis dis env) (value-of-con con env))))))

  (define value-of-con
    (lambda (con env)
      (cases conjunction con
        (conjuction-base (inv) (value-of-inv inv env))
        (conjunction-and (con inv) (and (value-of-inv inv env) (value-of-con con env))))))

  (define value-of-inv
    (lambda (inv env)
      (cases inversion inv
        (inversion-base (comp) (value-of-comp comp env))
        (inversion-not (inv) (not (value-of-inv inv)env)))))

  

  (define value-of-comp
    (lambda (comp env)
      (cases comparison comp
        (comparison-compare (s com-op-sum-pairs) (cases compare-op-sum-pairs com-op-sum-pairs
                                                   (compare-op-sum-pairs-base (cosp)
                                                                              (cases compare-op-sum-pair cosp
                                                                                (eq-sum (s2) (= (value-of-sum s env) (= (value-of-sum s2 env)))
                                                                                        (lt-sum (s2) (< (= (value-of-sum s env) (= (value-of-sum s2 env)))
                                                                                                        (gt-sum (s2) (> (= (value-of-sum s env) (= (value-of-sum s2 env)))))
                                                                                                        (compare-op-sum-pairs-multi (car-cosp cdr-cosp) (cases compare-op-sum-pair car-cosp
                                                                                                                                                          (eq-sum (s2) (= (= (value-of-sum s env) (value-of-comp (comparison-compare s2 cdr-cosp) env)))
                                                                                                                                                                  (lt-sum (s2) (< (= (value-of-sum s env) (value-of-comp (comparison-compare s2 cdr-cosp) env)))
                                                                                                                                                                          (gt-sum (s2) (> (= (value-of-sum s env) (value-of-comp (comparison-compare s2 cdr-cosp) env)))))))))))))))
        (comparison-base (s) (value-of-sum s env)))))                                         

  (define value-of-sum
    (lambda (s-dt env)
      (cases sum s-dt
        (sum-add (s t) (+ (value-of-sum s env) (value-of-term t env) ))
        (sum-subtract (s t) (- (value-of-sum s env) (value-of-term t env) ) )         
        (sum-base (t) (value-of-term t env)))))

  (define value-of-term
    (lambda (t env)
      (cases term t
        (term-multiplication (tt tf)  (* (value-of-term t env) (value-of-factor f v)))
        (term-division (tt tf)  (/ (value-of-term t env) (value-of-factor f env)))
        (term-base (tf)   (value-of-factor f env)))))

  
  (define value-of-factor
    (lamda (f)
           (cases factor f
             (factor-affirmation (ff) (value-of-factor ff))
             (factor-negation (ff) (- 0 (value-of-factor ff)))
             (factor-base (fp) (value-of-power fp)))))

  (define value-of-power
    (lambda (p env)
      (cases power p
        (power-pow (pa pf) (expt (value-of-atom pa env) (value-of-factor pf env)))
        (power-base (pprimary) (value-of-primary pprimary env)))))


  (define up-env-params
    (lambda (proc-params env)
       (cases params proc-params
         (parmas-base (param) (cases param-with-defualt param
                                (param-with-defualt-base (id exp) (extend-env lhs (newref (value-of-exp rhs env)) env))))
         (params-multi (car-param cdr-param)  (up-env-params cdr-param (cases param-with-defualt car-param
                                                      (param-with-defualt-base (id exp) (extend-env id (newref (value-of-exp exp env)) env))) )))))

  (define up-env-params-exps
    (lambda (proc-params exps env)
      
                                         
          
  (define call
    (lambda (function exps env)
      (cases proc function
        (py-proc (proc-name proc-params proc-stmts)   (value-of-exps exps (up-env-params-exps proc-params exps (up-env-params proc-params env)) ))))) 
      

  (define value-of-primary
    (lambda (pri env)
      (cases primary pri
        (primary-base (pa) (value-of-atom pa env))
        (primary-lst-index (pp pe) (list-ref (value-of-primary pp env) (value-of-exp pe env)))
        (primary-call-function-no-args (pp) (call (value-of-primary pp env) '() env))
        (primary-call-function (pp pa))
        


     
        (define value-of-stmt
          (lambda (stmt env)
            (cases statement statement
              (statement-simple-st (st)
                                   (cases simple-st st
                                     (assignment (lhs rhs)
                                                 (extend-env lhs (newref (value-of-exp rhs env)) env)                                           ))
  
                             
                                   
