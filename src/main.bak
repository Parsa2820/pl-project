
(module main racket
  (include "datatype .rkt")
  (require (lib "eopl.ss" "eopl"))
  
  (define report-invalid-reference
    (lambda (ref the-store)
      (error "invalid refrence")))

  (define apply-env
    (lambda (env search-sym)
      (cases environment env
        [extend-env (bvar bval saved-env) (if (eqv? search-sym bvar)
                                              bval
                                              (apply-env saved-env search-sym))])))
  
  (define reference?
    (lambda (v)
      (integer? v)))
  
  (define the-store 'uninitialized)

  (define empty-store
    (lambda ()
      '()))

  (define initialize-store!
    (lambda ()
      (set! the-store (empty-store))))

  (define newref
    (lambda (val)
      (let ([next-ref (length the-store)])
        (set! the-store (append the-store (list val)))
        next-ref)))

  (define deref
    (lambda (ref)
      (list-ref the-store ref)))

  (define setref!
    (lambda (ref val)
      (set! the-store
            (letrec ([setref-inner (lambda (store1 ref1)
                                     (cond [(null? store1) (report-invalid-reference ref the-store)]
                                           [(zero? ref1) (cons val (cdr store1))]
                                           [else (cons (car store1) (setref-inner (cdr store1) (- ref1 1)))]))])
              (setref-inner the-store ref)))))


  (define-datatype environment environment?
    [empty-env]
    [extend-env [bvar symbol?]
                [bval reference?]
                [saved-env environment?]])


  (define value-of-stmts
    (lambda (statements)
      if (null? statements)
      (begin (value-of-stmt (car statements)) (value-of-stmts (cdr statements)))
      
      )
    )

 
  (define value-of-exp
    (lambda (exp)
      (cases expression exp
        (expression-base 

         ))))


     
  (define value-of-stmt
    (lambda (stmt)
      (cases statement statement
        (statement-simple-st (st)
                             (cases simple-st st
                               (assignment (lhs rhs)
                                           (setref! (value-of-exp (exp->stmt rhs)) (apply-env lhs))
       
                                           ))
  
                             
