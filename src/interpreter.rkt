(module interpreter racket
  
  (require (lib "eopl.ss" "eopl"))
  (require "parser.rkt") 
  (require "datatype.rkt")
  (require "environment.rkt")
  (require "store.rkt")

  (provide (all-defined-out))
  
  (define envs-stack (list))
  (define current-env (empty-env))

  (define (initialize-envs!)
    (begin
      (set! envs-stack (list))
      (set! current-env (empty-env)))
    )

  (define (get-global-env-val)
    (list-ref envs-stack (- (length envs-stack) 1))
    )

  (define (push-current-env-to-envs-stack-and-set-current-env env)
    (begin
      (set! envs-stack (cons current-env envs-stack))
      (set! current-env env))
    )

  (define (pop-envs-stack-to-current-env)
    (begin
      (set! current-env (car envs-stack))
      (set! envs-stack (cdr envs-stack)))
    )

  (define value-of-program
    (lambda (prgm)
      (cases program prgm
        (program-base (sts) (value-of-stmts sts))))
    )

  (define value-of-stmts
    (lambda (stmts)
      (cases statements stmts      
        (statements-base (st) (value-of-stmt st))
        (statements-multi (car-st cdr-st) (begin (value-of-stmts cdr-st) (value-of-stmt car-st)))))
    )
       
  (define value-of-stmt
    (lambda (stmt)
      (cases statement stmt
        (statement-simple-st (st) (value-of-simple-st st))
        (statement-compound-st (st) (value-of-compound-st st))))
    )

  (define value-of-simple-st
    (lambda (st)
      (cases simple-st st
        (assignment-st (lhs rhs) (value-of-assignment-st lhs rhs))
        (return-st (return-type) (value-of-return-datatype return-type))
        (global-st (id)
                   (begin (displayln 'global-st-call) (set! current-env
                         (extended-env (symbol-append '@global- id) (apply-env id (get-global-env-val))
                                       (extend-env id (apply-env id (get-global-env-val)) current-env)))))
        (pass-st () (void))
        (break-st () (value-of-break-st))
        (continue-st () (value-of-continue))
        (print-st (vals) (print-atoms-lst vals))
        (else (void))))
    )

  (define (value-of-assignment-st lhs rhs)
    (let ([global-ref (apply-env (symbol-append '@global- lhs) current-env)])
      (if (null? global-ref)
          (set! current-env (extend-env lhs (newref (identifier-datatype-lazy rhs current-env)) current-env))
          (setref! global-ref (identifier-datatype-lazy rhs current-env))
          ))
    )

  (define (value-of-return-datatype return-type)
    (cases return-datatype return-type
      (return-void () (begin (setref! (apply-env '@return current-env) #t) (set! current-env (extend-env '@returnvalue (newref 0) current-env))))
      (return-exp (exp)
                  (begin
                    (setref! (apply-env '@return current-env) #t)
                    (set! current-env (extend-env '@returnvalue (newref (value-of-exp exp)) current-env)))))
    )
  
  (define value-of-break-st
    (lambda ()
      (setref! (apply-env '@break current-env) #t))
    )

  (define value-of-continue
    (lambda ()
      (setref! (apply-env '@continue current-env) #t ))
    )

  (define (print-atoms-lst atoms-lst)
    (cond
      [(null? atoms-lst) (void)]
      [else
       (begin
         (print-atom (value-of-atom (car atoms-lst)))
         (display " ")
         (print-atoms-lst (cdr atoms-lst)))])
    )

  (define (print-atom a)
    (cond
      [(equal? a 'None) (display "None")]
      [(list? a) (begin (display "[") (print-atom-lst a) (display "]"))]
      [(boolean? a) (if a (display "True") (display "False"))]
      [else (display a)]
      )
    )

  (define (print-atom-lst lst)
    (if (null? lst)
        (void)
        (begin
          (print-atom (car lst)) (if (null? (cdr lst)) (void) (display ", ")) (print-atom-lst (cdr lst))))              
    )

  (define value-of-compound-st
    (lambda (st)
      (cases compound-st st
        (function-def-st (func) (value-of-fun func))
        (if-st (exp true-stmts false-stmts) (if (value-of-exp exp) (value-of-stmts true-stmts) (value-of-stmts false-stmts)))
        (for-st (id exp stmts) (begin
                                 (set! current-env (extend-env '@continue (newref #f) current-env))
                                 (set! current-env (extend-env '@break (newref #f) current-env))
                                 (value-of-for id (value-of-exp exp) stmts)
                                 (set! current-env (extend-env '@break (newref #f) current-env))
                                 (set! current-env (extend-env '@continue (newref #f) current-env))))))
    )

  (define value-of-for
    (lambda (id expval stmts) 
      (if (null?  expval)
          (void)
          (begin          
            (if (not (deref (apply-env '@break current-env)))
                (begin
                  (set! current-env (extend-env '@continue (newref #f) current-env))
                  (set! current-env (extend-env id (newref (identifier-datatype-value (car expval))) current-env))
                  (value-of-for-stmts stmts))
                (void))
            (value-of-for id (cdr expval) stmts))
          ))
    )

  (define value-of-for-stmts
    (lambda (stmts)
      (cases statements stmts      
        (statements-base (st) (value-of-stmt st))
        (statements-multi (car-st cdr-st) (begin
                                            (value-of-for-stmts cdr-st)                                                                              
                                            (if (and (not (deref (apply-env '@break current-env))) (not (deref (apply-env '@continue current-env))))
                                                (value-of-stmt car-st)
                                                (void))))))
    )
      
  (define value-of-fun
    (lambda (func)
      (cases function-datatype func       
        (function-no-input (id stmts) (set! current-env (extend-env id (newref (identifier-datatype-value func)) current-env)))
        (function-with-input (id params sts) (set! current-env (extend-env id (newref (identifier-datatype-value func)) current-env)))))
    )
 
  (define value-of-exp
    (lambda (exp)
      (cases expression exp
        (expression-base (dis) (value-of-dis dis))))
    )

  (define value-of-dis
    (lambda (dis)
      (cases disjunction dis
        (disjunction-base (con) (value-of-con con))
        (disjunction-or (con dis) (or (value-of-dis dis) (value-of-con con)))))
    )

  (define value-of-con
    (lambda (con)
      (cases conjunction con
        (conjunction-base (inv) (value-of-inv inv))
        (conjunction-and (con inv) (and (value-of-inv inv) (value-of-con con)))))
    )

  (define value-of-inv
    (lambda (inv)
      (cases inversion inv
        (inversion-base (comp) (value-of-comp comp))
        (inversion-not (inv) (not (value-of-inv inv)))))
    )

  (define value-of-comp
    (lambda (comp)
      (cases comparison comp
        (comparison-compare (s com-op-sum-pairs) (cases compare-op-sum-pairs com-op-sum-pairs
                                                   (compare-op-sum-pairs-base (cosp)
                                                                              (cases compare-op-sum-pair cosp
                                                                                (eq-sum (s2) (equal? (value-of-sum s) (value-of-sum s2)))
                                                                                (lt-sum (s2) (< (value-of-sum s) (value-of-sum s2)))
                                                                                (gt-sum (s2) (> (value-of-sum s)  (value-of-sum s2)))))
                                                   (compare-op-sum-pairs-multi (car-cosp cdr-cosp) (cases compare-op-sum-pair car-cosp
                                                                                                     (eq-sum (s2) (equal?  (value-of-sum s) (value-of-comp (comparison-compare s2 cdr-cosp))))
                                                                                                     (lt-sum (s2) (<  (value-of-sum s) (value-of-comp (comparison-compare s2 cdr-cosp))))
                                                                                                     (gt-sum (s2) (>  (value-of-sum s) (value-of-comp (comparison-compare s2 cdr-cosp))))))))
        (comparison-base (s) (value-of-sum s))))
    )

  (define value-of-sum
    (lambda (s-dt)
      (cases sum s-dt
        (sum-add (s t) (+pro (value-of-sum s) (value-of-term t) ))
        (sum-subtract (s t) (- (value-of-sum s) (value-of-term t) ) )         
        (sum-base (t) (value-of-term t))))
    )

  (define (+pro a b)
    (cond
      [(and (boolean? a) (boolean? b)) (or a b)]
      [(and (number? a) (number? b)) (+ a b)]
      [(and (list? a) (list? b)) (append a b)]
      )
    )

  (define value-of-term
    (lambda (t)
      (cases term t
        (term-multiplication (tt tf)
                             (let ((left (value-of-term tt)))
                               (if (or (equal? left 0) (equal? left #f))
                                left
                                (*pro left (value-of-factor tf)))))
        (term-division (tt tf)  (exact->inexact (/ (value-of-term tt) (value-of-factor tf))))
        (term-base (tf)   (value-of-factor tf))))
    )
  
  (define (*pro a b)
    (cond
      [(and (boolean? a) (boolean? b)) (and a b)]
      [(and (number? a) (number? b)) (* a b)]
      )
    )
  
  (define value-of-factor
    (lambda (f)
      (cases factor f
        (factor-affirmation (ff) (value-of-factor ff))
        (factor-negation (ff) (- 0 (value-of-factor ff)))
        (factor-base (fp) (value-of-power fp))))
    )

  (define value-of-power
    (lambda (p)
      (cases power p
        (power-pow (pa pf) (expt (value-of-atom pa) (value-of-factor pf)))
        (power-base (pprimary) (value-of-primary pprimary))))
    )
  
  (define value-of-primary
    (lambda (pri)
      (cases primary pri
        (primary-base (pa) (value-of-atom pa))
        (primary-lst-index (pp pe) (list-ref (value-of-primary pp) (inexact->exact (value-of-exp pe))))
        (primary-call-function-no-args (pp) (call-no-input (value-of-primary pp) ))
        (primary-call-function (pp pa) (call-with-input (value-of-primary pp) pa))
        (else (void))
        )
      )
    )

  (define call-no-input
    (lambda (function)
      (cases function-datatype function
        (function-no-input (name stmts)
                           (begin
                             (push-current-env-to-envs-stack-and-set-current-env (create-function-no-input-call-no-input-env name))
                             (value-of-func-stmts stmts)
                             (let ([return-value (deref (apply-env '@returnvalue current-env))])
                               (begin (pop-envs-stack-to-current-env) return-value))
                             ))
        (function-with-input (name parameters stmts)
                           (begin
                             (push-current-env-to-envs-stack-and-set-current-env (create-function-with-input-call-no-input-env name parameters))
                             (value-of-func-stmts stmts)
                             (let ([return-value (deref (apply-env '@returnvalue current-env))])
                               (begin (pop-envs-stack-to-current-env) return-value))
                             ))))
    )

  (define call-with-input
    (lambda (function args)
      (cases function-datatype function
        (function-with-input (name parameters stmts)
                           (begin
                             (push-current-env-to-envs-stack-and-set-current-env (create-function-with-input-call-with-input-env name parameters args))
                             (value-of-func-stmts stmts)
                             (let ([return-value (deref (apply-env '@returnvalue current-env))])
                               (begin (pop-envs-stack-to-current-env) return-value))                           
                             ))
        (else (void))))
    )

  (define (create-function-with-input-call-with-input-env name parameters args)
    (add-function-arguments-to-env args parameters (create-function-with-input-call-no-input-env name parameters))
    )

  (define (add-function-arguments-to-env args parameters env)
    (cases arguments args
      (arguments-base (exp)
                      (cases params parameters
                        (params-base (param) (add-function-argument-to-env exp param env))
                        (params-multi (car-param cdr-param)
                                      (add-function-defualt-parameters-to-env cdr-param (add-function-argument-to-env exp car-param env)))))
      (arguments-multi (car-arg cdr-arg)
                       (cases params parameters                        
                        (params-multi (car-param cdr-param)
                                      (add-function-arguments-to-env cdr-arg cdr-param (add-function-argument-to-env car-arg car-param env)))
                        (else (void)))))
    )

  (define (create-function-no-input-call-no-input-env name)
    (extend-env name (apply-env name current-env) (extended-env '@return (newref #f) (empty-env)))
    )

  (define (create-function-with-input-call-no-input-env name parameters)
    (add-function-defualt-parameters-to-env parameters (create-function-no-input-call-no-input-env name))
    )

  (define (add-function-defualt-parameters-to-env parameters env)
    (cases params parameters
      (params-base (param) (add-function-defualt-parameter-to-env param env))
      (params-multi (car-param cdr-param)
                    (add-function-defualt-parameters-to-env cdr-param (add-function-defualt-parameter-to-env car-param env))))
    )

  (define (add-function-defualt-parameter-to-env parameter env)
    (cases param-with-defualt parameter
                     (param-with-defualt-base (id exp)
                                              (extend-env id (newref (identifier-datatype-lazy exp current-env)) env)))
    )

  (define (add-function-argument-to-env arg-exp parameter env)
    (cases param-with-defualt parameter
                     (param-with-defualt-base (id exp)
                                              (extend-env id (newref (identifier-datatype-lazy arg-exp current-env)) env)))
    )

  (define (value-of-func-stmts stmts)
    (cases statements stmts      
      (statements-base (st) (value-of-stmt st))
      (statements-multi (car-st cdr-st)
                        (begin
                          (value-of-func-stmts cdr-st)
                          (if (not (deref (apply-env '@return current-env)))
                              (value-of-stmt car-st)
                              (void)))))
    )

  (define value-of-atom
    (lambda (_atom)
      (cases atom _atom
        (atom-identifier (id) (value-of-identifier-datatype (deref (apply-env id current-env))))
        (atom-bool (val) val)
        (atom-none () 'None)
        (atom-number (val) val)
        (atom-lst (val) (value-of-lst val))))
    )

  (define debug-exit 0)

  (define (value-of-identifier-datatype id-datatype)
    (cases identifier-datatype id-datatype
      (identifier-datatype-value (val) val)
      (identifier-datatype-lazy (exp saved-env) (begin
                                        (push-current-env-to-envs-stack-and-set-current-env saved-env)
                                        (let ([val (value-of-exp exp)])
                                          (begin (pop-envs-stack-to-current-env) val)))))
    )

  (define value-of-lst
    (lambda (_lst)
      (cases lst _lst
        (empty-lst () '())
        (not-empty-lst (exps) (value-of-exps exps))))
    )

  (define value-of-exps
    (lambda (_exps)
      (cases expressions _exps
        (expressions-base (exp) (list (value-of-exp exp)))
        (expressions-multi (car-exp cdr-exp)
                           (append (value-of-exps cdr-exp) (list (value-of-exp car-exp))))))
    )

  (define (symbol-append sym1 sym2)
    (string->symbol (string-append (symbol->string sym1) (symbol->string sym2)))
    )

  (define (evaluate-file path)
    (run (file->string path))
    )

  (define (run pgm-string)
    (define py-lexer (lex-this python-lexer (open-input-string pgm-string)))
    (begin (initialize-store!)
           (initialize-envs!)
           (let ((parser-res (python-parser py-lexer)))
              (value-of-program parser-res))
           )
    )
)
