; Allen Liu, Steven Feng, Xingheng Lin

; Problem #4

(load "chez-init.ss") ; put this file in the same folder, or add a pathname

; This is a parser for simple Scheme expressions, 
; such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

(define-datatype expression expression?
  [var-exp
   (id symbol?)]
  [lambda-exp
   (ids (list-of symbol?))
   (bodies (list-of expression?))]
  [lambda-x-exp
    (id symbol?)
    (bodies (list-of expression?))]
  [lambdaImp-exp 
    (ids pair?)
    (bodies (list-of expression?))]
  [lit-exp 
    (lit (lambda (m) #t))]
  [app-exp
    (rator expression?)
    (rands (list-of expression?))]
  [set!-exp
    (id symbol?)
    (val-exp expression?)]
  [ifelse-exp 
    (ifpred expression?)
    (ifdot  expression?)
    (ifdof  expression?)]
  [if-exp
    (ifpred expression?)
    (ifdot  expression?)]
  [namedlet-exp
    (name symbol?)
    (var-list (list-of (list-of expression?)))
    (body (list-of expression?))]
  [let-exp
    (var-list (list-of (list-of expression?)))
    (body (list-of expression?))]
  [let*-exp
    (var-list (list-of (list-of expression?)))
    (body (list-of expression?))]
  [letrec-exp
    (var-list (list-of (list-of expression?)))
    (body (list-of expression?))])
 
(define-datatype lit-type lit-type?
  [an-number
    (num number?)]
  [an-string
    (str string?)]
  [an-bool
    (b boolean?)]
  [an-char
    (c char?)])
    
    

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
                         [else (let-exp (map (lambda (x)   
                                               (map parse-exp x)) 
                                             (2nd datum)) 
                                        (map parse-exp (cddr datum)))])]
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
                  [(eqv? (1st datum) 'quote)
		   (lit-exp (cadr datum))]
		  [else (app-exp (parse-exp (1st datum)) 
                                 (map parse-exp (cdr datum)))])]
          [else (eopl:error 'parse-exp "bad expression: ~s" datum)])))


(define unparse-exp
  (lambda  (datum)
 li   (cases expression datum
      [var-exp (id)  id]
      [lit-exp (num) num]
      [lambda-exp (ids bodies) (append (list 'lambda ids) 
                                       (map unparse-exp bodies))]
      [lambda-x-exp (id bodies) (append (list 'lambda id) 
                                        (map unparse-exp bodies))]
      [lambdaImp-exp (ids bodies) (append (list 'lambda ids) 
                                          (map unparse-exp  bodies))]
      [set!-exp (id val-exp) (list 'set! 
                                   id 
                                   (unparse-exp val-exp))]
      [let-exp (var-list bodies) (append (list 'let (map (lambda (x)
                                                           (map unparse-exp x)) 
                                                         var-list)) 
                                         (map unparse-exp bodies))]
      [namedlet-exp (name var-list bodies) (append (list 'let name 
                                                         (map (lambda (x)
                                                                (map unparse-exp x)) 
                                                              var-list)) 
                                                   (map unparse-exp bodies))]
      [letrec-exp (var-list bodies) (append (list 'letrec 
                                                  (map (lambda (x)
                                                         (map unparse-exp x)) 
                                                       var-list)) 
                                            (map unparse-exp bodies))]
      [let*-exp (var-list bodies) (append (list 'let* 
                                                (map (lambda (x)
                                                       (map unparse-exp x)) 
                                                     var-list)) 
                                          (map unparse-exp bodies))]
      [if-exp (ifpred ifdot) (list 'if 
                                   (unparse-exp ifpred) 
                                   (unparse-exp ifdot))]
      [ifelse-exp (ifpred ifdot ifdof) (list 'if 
                                             (unparse-exp ifpred) 
                                             (unparse-exp ifdot) 
                                             (unparse-exp ifdof))] 
      [app-exp (rator rands) (append (list (unparse-exp rator)) 
                                     (map unparse-exp rands))])))
      
; An auxiliary procedure that could be helpful.
(define var-exp?
 (lambda  (x)
   (cases expression x
     [var-exp (id) id]
     [lit-exp (num) num]
     [else #f])))
(var-exp? (var-exp 'a))
;(var-exp? (app-exp (var-exp 'a) (var-exp 'b)))
