; When you write syntax-expand for A14, this could be a good place to pt your code.

(define syntax-expand 
	(lambda (exp)
	  (cases expression exp
		  [cond-exp  (body)
        (let cond-to-if ([body body])
          (let ([pred (syntax-expand (caar body))]
                [to-do (syntax-expand (cadar body))])
            (if (null? (cdr body))
              (if (equal? (var-exp 'else) pred)
                to-do
                (if-exp pred to-do))
              (ifelse-exp pred
                          to-do
                          (cond-to-if (cdr body))))))]
	    [or-exp (body)
        (let or-to-if ([body body])
          (if (null? body)
            (lit-exp #f)
            (if (null? (cdr exp))
              (car body)
              (syntax-expand (let-exp (list 'tmp)
                                      (list (syntax-expand (car body)))
              (list (ifelse-exp (var-exp 'tmp)
                          (var-exp 'tmp)
                          (or-to-if (cdr body)))))))))]
	    [and-exp (body)
        (let and-to-if ([body body])
          (if (null? body)
            (lit-exp #t)
            (ifelse-exp (syntax-expand (car body))
                        (and-to-if (cdr body))
                        (lit-exp #f))))]
	    [case-exp (id cases evals)
        (syntax-expand (let-exp ;(list (list (lit-exp 'temp) id))
          (list 'temp)
          (list id)
          (list (let case-to-if ([cases cases]
                         [evals evals])
          (if (null? cases)
            (if (not (null? evals))
              (begin-exp (1st evals))
              (app-exp (var-exp 'void) '()))
            (ifelse-exp (app-exp (var-exp 'memq) (list (var-exp 'temp) (lit-exp (car cases))))
                        (begin-exp (car evals))
                        (case-to-if (cdr cases) (cdr evals))))))))]
         ; (let ([val (caar body)]
         ;       [to-do (cadar body)])
         ;   (if (null? (cdr body))
         ;     (if (list? val)
         ;       (if-exp (apply or-exp (map (lambda (x) 
         ;                                    (app-exp (var-exp 'eq?)
         ;                                             (list (syntax-expand id)
         ;                                                   (syntax-expand x))))
         ;                                  val))
         ;               to-do)
           ;     (if-exp (app-exp (var-exp 'eq?) 
         ;;                      (list id val))
             ;         to-do))
            ;  (ifelse-exp (app-exp (var-exp 'eq?)
              ;                     (list id val))
               ;           to-do
                ;          (case-to-if (cdr body))))))]
	    
      [begin-exp (body)
        (syntax-expand (app-exp (lambda-exp '() body) '()))]  
      [lambda-exp (id body)
        (lambda-exp id (map syntax-expand body))]
      [lambda-x-exp (ids bodies)
        (lambda-x-exp ids (map syntax-expand bodies))]
      [lambdaImp-exp (ids bodies)
        (lambdaImp-exp ids (map syntax-expand bodies))]
      [if-exp (pred dot)
        (if-exp (syntax-expand pred) 
                (syntax-expand dot))]
      [ifelse-exp (pred dot dof)
        (ifelse-exp (syntax-expand pred) 
                    (syntax-expand dot) 
                    (syntax-expand dof))]
      [app-exp (rator rands)
        (app-exp (syntax-expand rator)
                 (map syntax-expand rands))]
      [let-exp (vars args body)
        (syntax-expand (app-exp (lambda-exp vars body) args))]
      [let*-exp (vars args body)
        (syntax-expand (let-exp (list (car vars))
                                (list (car args))
                                (if (null? (cdr vars))
                                  body
                                  (list (let*-exp (cdr vars)
                                            (cdr args)
                                            body)))))]
      [else exp] 
       )))