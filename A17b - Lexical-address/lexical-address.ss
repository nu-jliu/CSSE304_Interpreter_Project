(define lexical-address
  (lambda (exp)
    (let helper ([exp exp]
                 [bound '()])
    (cases expression exp
      [lit-exp (datum) exp]
      [var-exp (id) 
        (var-exp (get-address id bound 0))]
      [lambda-exp (id body)
        (lambda-exp id (map (lambda (x) 
                              (helper x (cons id bound))) 
                            body))]
      [lambda-x-exp (ids bodies)
        (lambda-x-exp ids (map (lambda (x) 
                                 (helper x (cons ids bound))) 
                               bodies))]
      [lambdaImp-exp (ids bodies)
        (lambdaImp-exp ids (map (lambda (x) 
                                  (helper x (cons ids bound))) 
                                bodies))]
      [case-lambda-exp (idss bodiess)
        (case-lambda-exp idss (map (lambda (x) 
                                     (helper x (cons idss bound))) 
                                   bodiess))]
      [if-exp (ifpred ifdot)
        (if-exp (helper ifpred bound) 
                (helper ifdot bound))]
      [ifelse-exp (ifpred ifdot ifdof)
        (ifelse-exp (helper ifpred bound) 
                    (helper ifdot bound)
                    (helper ifdof bound))]
      [app-exp (rator rands)
        ; (let ([proc-value (eval-exp rator env)]
        ;       [args (eval-rands rands env)])
        ;   (apply-proc proc-value args))
        (app-exp (helper rator bound) 
                 (map (lambda (x) 
                        (helper x bound)) 
                      rands))]
      [while-exp (test-exp bodies)
        ; (let loop ([t test-exp])
        ;   (if (eval-exp t env)
        ;     (begin (eval-bodies bodies env) 
        ;            (loop t))
        ;     (eval-exp (app-exp (var-exp 'void) '()) env)))
        (while-exp (helper test-exp bound) 
                   (map (lambda (x) 
                     (helper x bound)) 
                   bodies))]
      [letrec-exp (proc-names idss bodiess letrec-bodies)
        (letrec-exp proc-names
                    idss
                    (apply map (lambda (x y) 
                                 (map (lambda (z) 
                                        (helper z 
                                                (cons y 
                                                      (cons proc-names bound)))) 
                                      x)) 
                               (list bodiess idss))
                    (map (lambda (x) 
                           (helper x 
                                   (cons proc-names bound))) 
                         letrec-bodies))]
      [set!-exp (id exp)
        (set!-exp id
                  (helper exp bound))]
      [define-exp (id val)
        ;(if (contains-env env id)
          ; (error 'eval-exp
          ;        "~a is already in environment"
          ;        id)
          ; (add-global-environment (cons id bound) 
          ;                         (list (eval-exp val env)))
          (define-exp id (helper val bound))]
      [else (eopl:error 'lexical-address "Bad abstract syntax: ~a" exp)]))))

(define get-address
  (lambda (sym bound depth)
    (cond [(null? bound) (free-lexical sym)]
          [(>= (get-pos sym (car bound)) 0) (bound-lexical depth 
                                                           (get-pos sym (car bound)))]
          [else (get-address sym (cdr bound) 
                                 (+ 1 depth))])))

(define get-pos
  (lambda (ele lst)
    (cond [(null? lst) -1]
          [(eq? ele (car lst)) 0]
          [else (if (eq? -1 (get-pos ele (cdr lst)))
                  -1
                  (+ 1 (get-pos ele (cdr lst))))])))