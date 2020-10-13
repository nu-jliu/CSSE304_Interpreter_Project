; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp         
  (lambda (datum)
    (cond [(symbol? datum) (var-exp datum)]
          [(number? datum) (lit-exp datum)]
          [(boolean? datum) (lit-exp datum)]
          [(char? datum) (lit-exp datum)]
          [(string? datum) (lit-exp datum)]
          [(null? datum) (lit-exp datum)]
          [(vector? datum) (lit-exp datum)]
          [(pair? datum)
            (cond [(not (list? datum)) (eopl:error 'parse-exp 
                                                   "application ~s is not a proper list" 
                                                   datum)]  
                  [(eqv? (1st datum) 'lambda)
                    (cond [(< (length datum) 3) (eopl:error 'parse-exp 
                                                            "bad input for lambda : ~s" 
                                                            datum)]
                          [(symbol? (2nd datum))
                            (lambda-x-exp (2nd datum)
                		                      (map parse-exp (cddr datum)))]
                          [(not (andmap symbol? (2nd datum)))  (eopl:error 'parse-exp 
                                                                           "lambda argument list: formals must be symbols: ~s" 
                                                                           datum)]
                          ;[(symbol? (3rd datum)) ]
                          [(list? (2nd datum)) (lambda-exp (2nd datum)
                                                           (if (symbol? (3rd datum))
                                                             (parse-exp (3rd datum))
                                                             (map parse-exp (cddr datum))))]
                          [else (lambdaImp-exp (2nd datum)  
                                               (map parse-exp (cddr datum)))])]
                  [(eqv? (1st datum) 'set!)
                    (if (not (= 3 (length datum)))
                      (eopl:error 'parse-exp 
                                  "bad input for set! : ~s" 
                                  datum)
                      (set!-exp (2nd datum) (parse-exp (3rd datum))))]
                  [(eqv? (1st datum) 'if)
                    (cond [(or (< (length datum) 3) (> (length datum) 4))
                            (eopl:error 'parse-exp 
                                        "bad input for if: ~s" 
                                        datum)]
                          [(= 3 (length datum)) (if-exp (parse-exp (2nd datum)) 
                                                        (parse-exp (3rd datum)))]
                          [else (ifelse-exp (parse-exp (2nd datum))  
                                            (parse-exp (3rd datum)) 
                                            (parse-exp (cadddr datum)))])]
                  [(eqv? (1st datum) 'let)
                    (cond [(< (length datum) 3) (eopl:error 'parse-exp 
                                                            "bad input for let: ~s" 
                                                            datum)]
                          [(symbol? (2nd datum)) (namedlet-exp (2nd datum) 
                                                               (map (lambda (x)   
                                                                      (map parse-exp x)) 
                                                                    (3rd datum)) 
                                                               (map parse-exp (cdddr datum)))]
                          [(not (list? (2nd datum))) (eopl:error 'parse-exp 
                                                                 "let* declarations not a list: ~s"
                                                                 datum)]
                          [(not (andmap (lambda (x) 
                                          (and (list? x) 
                                               (= 2 (length x)) 
                                               (symbol? (car x)))) 
                                        (2nd datum))) (eopl:error 'parse-exp 
                                                                  "not a proper list: ~s" 
                                                                  datum)]
                         [else (parse-exp (let->application datum))])]
                  [(eqv? (1st datum) 'let*)
                    (cond [(< (length datum) 3) (eopl:error 'parse-exp 
                                                            "bad input for let*: ~s" 
                                                            datum)]
                          [(not (list? (2nd datum))) (eopl:error 'parse-exp 
                                                                 "let* declarations not a list: ~s"
                                                                 datum) ]
                          [(not (andmap (lambda (x) 
                                          (and (list? x) 
                                               (= 2 (length x)) 
                                               (symbol? (car x)))) 
                                        (2nd datum))) (eopl:error 'parse-exp 
                                                                  "not a proper list:  ~s" 
                                                                  datum)]
                          [else (let*-exp (map (lambda (x)   
                                                 (map parse-exp x))  
                                               (2nd datum)) 
                                          (map parse-exp (cddr datum)))])]
                  [(eqv? (1st datum) 'letrec)
                    (cond [(< (length datum) 3) (eopl:error 'parse-exp 
                                                            "bad input for letrec: ~s" 
                                                            datum)]
 				                  [(not (list? (2nd datum))) (eopl:error 'parse-exp 
                                                                 "let* declarations not a list: ~s"
                                                                 datum) ]
                          [(not (andmap (lambda (x) 
                                          (and (list? x) 
                                               (= 2 (length x)) 
                                               (symbol? (car x)))) 
                                        (2nd datum))) (eopl:error 'parse-exp 
                                                                  "not a proper list: ~s" 
                                                                  datum)]
                          [else (letrec-exp (map (lambda (x)   
                                                   (map parse-exp x)) 
                                                 (2nd datum)) 
                                            (map parse-exp (cddr datum)))])]
                  [(eqv? (1st datum) 'quote) (lit-exp (cadr datum))]
		              [else (app-exp (parse-exp (1st datum)) 
                                 (map parse-exp (cdr datum)))])]
          [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))










