
;; Parsed expression datatype.  You will probably want to replace this
;; with your expression datatype from A11b.

; (define-datatype expression expression?
;   [var-exp        ; variable references
;    (id symbol?)]
;   [lit-exp        ; "Normal" data.  Did I leave out any types?
;     (datum
;       (lambda (x) 
;         (ormap (lambda (pred) 
;                  (pred x))
;                (list number? vector? boolean? symbol? string? pair? null?))))]
;   [app-exp        ; applications
;     (rator expression?)
;     (rands (list-of expression?))])

 	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vals (list-of cell?))
    (env environment?)))

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
    (name symbol?)]
  [closure
    (ids (lambda (x) (or (symbol? x) 
                         (pair? x) 
                         ((list-of symbol?) x))))
    (bodies (lambda (x) (or ((list-of expression?) x)
                            (expression? x))))
    (env environment?)]
  [closure-list 
    (idss (list-of (lambda (x) (or (symbol? x) 
                                   (pair? x) 
                                   ((list-of symbol?) x)))))
    (bodiess (list-of (lambda (x) (or ((list-of expression?) x)
                                      (expression? x)))))
    (env environment?)])
(define-datatype lexical lexical?
  [free-lexical 
    (sym symbol?)]
  [bound-lexical  
    (dep number?)
    (pos number?)])

(define-datatype expression expression?
  [var-exp
   (id (lambda (x) 
         (or (symbol? x) 
             (lexical? x))))]
  [lambda-exp
   (ids (list-of symbol?))
   (bodies (lambda (x) 
             (or ((list-of expression?) x) 
                 (expression? x))))]
  [lambda-x-exp
    (id symbol?)
    (bodies (list-of expression?))]
  [for-exp 
    (init (list-of expression?))
    (test expression?)
    (update (list-of expression?))
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
    (ifdot expression?)
    (ifdof expression?)]
  [if-exp
    (ifpred expression?)
    (ifdot expression?)]
  [namedlet-exp
    (name symbol?)
    (vars (list-of symbol?))
    (args (list-of expression?))
    (body (list-of expression?))]
  [let-exp
    (vars (list-of symbol?))
    (args (list-of expression?))
    (body (list-of expression?))]
  [let*-exp
    (vars (list-of symbol?))
    (args (list-of expression?))
    (body (list-of expression?))]
  [letrec-exp
    (proc-names (list-of symbol?))
    (idss (list-of (list-of symbol?)))
    (bodiess (list-of (list-of expression?)))
    (letrec-bodies (list-of expression?))]
  [begin-exp 
    (body (list-of expression?))]
  [cond-exp 
    (body (list-of (list-of expression?)))]
  [case-exp
    (id expression?)
    (cases (list-of list?))
    (evals (list-of (list-of expression?)))]
  [case-lambda-exp
    (idss (list-of (lambda (x) 
                     (or (symbol? x)
                         (and (pair? x)
                              (symbol? (car x)))
                         ((list-of symbol?) x)))))
    (bodiess (list-of (list-of expression?)))]
  [or-exp
    (body (list-of expression?))]
  [and-exp
    (body (list-of expression?))]
  [while-exp
    (test-exp expression?)
    (bodies (list-of expression?))]
  [define-exp
    (id symbol?)
    (val expression?)])
 
(define-datatype lit-type lit-type?
  [an-number
    (num number?)]
  [an-string
    (str string?)]
  [an-bool
    (b boolean?)]
  [an-char
    (c char?)])

(define cell box)
(define cell? box?)
(define cell-ref unbox)
(define cell-set! set-box!)