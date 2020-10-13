; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env)))

; eval-exp is the main component of the interpreter
(define eval-helper
  (lambda (body env)
    (if (null? (cdr body))
        (eval-exp (car body) env)
        (begin
          (eval-exp (car body) env)
          (eval-helper (cdr body) env)))))
    
(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
	(apply-env env id)]
      [lambda-exp (id body)
        (closure id body env)]
      [if-exp (ifpred ifdot)
        (if (eval-exp ifpred env) (eval-exp ifdot env))]
      [ifelse-exp (ifpred ifdot ifdof)
        (if (eval-exp ifpred env) (eval-exp ifdot env) (eval-exp ifdof env))]
      [let-exp (var-list body)
        (let ([syms (map (lambda (x) (eval-exp (1st x) env)) var-list)]
             [vars (map (lambda (x) (eval-exp (2nd x) env)) var-list)])
          (begin (display syms) (display vars)
        (eval-exp body (extend-env syms vars env))))]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-rands rands env)])
          (apply-proc proc-value args))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-exp x env)) rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      [closure (id body env) (eval-helper body (extend-env id args env))]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * add1 sub1 cons = / zero? not < > <= >= car cdr caar cadr cdar cddr 
                            caaar caadr cadar caddr cdaar cdadr cddar cdddr list null? assq eq? 
                            equal? atom? length list->vector list? pair? procedure? vector->list vector
                            make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set!
                            display newline
                            ))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      ;[(+) (+ (1st args) (2nd args))]
      ;[(-) (- (1st args) (2nd args))]
      ;[(*) (* (1st args) (2nd args))]
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(add1) (+ (1st args) 1)]
      [(sub1) (- (1st args) 1)]
      [(cons) (cons (1st args) (2nd args))]
      [(=) (= (1st args) (2nd args))]
      [(zero?) (if (null? (cdr args))
                   (zero? (1st args))
                   (eopl:error 'apply-prim-proc "Incorrect number of arguments: ~s" prim-op))]
      [(not) (not (1st args))];
      [(<) (apply < args)]
      [(>) (apply > args)]
      [(<=) (apply <= args)]
      [(>=) (apply >= args)]
      [(car) (car (1st args))];
      [(cdr) (cdr (1st args))];
      [(caar) (caar (1st args))];
      [(cadr) (cadr (1st args))];
      [(cdar) (cdar (1st args))];
      [(cddr) (cddr (1st args))];
      [(caaar) (caaar (1st args))];
      [(caadr) (caadr (1st args))];
      [(cadar) (cadar (1st args))];
      [(caddr) (caddr (1st args))];
      [(cdaar) (cdaar (1st args))];
      [(cdadr) (cdadr (1st args))];
      [(cddar) (cddar (1st args))];
      [(cdddr) (cdddr (1st args))];
      [(list) args]
      [(null?) (null? (1st args))];
      [(assq) (assq (1st args) (2nd args))];
      [(eq?) (eq? (1st args) (2nd args))];
      [(equal?) (equal? (1st args) (2nd args))];
      [(atom?) (atom? (1st args))];
      [(length) (length (1st args))];
      [(list->vector) (list->vector (1st args))];
      [(list?) (list? (1st args))];
      [(pair?) (pair? (1st args))];
      [(procedure?) (proc-val? (1st args))];
      [(vector->list) (vector->list (1st args))];
      [(vector) (apply vector args)]
      [(make-vector) (if (not (number? (1st args)))
                         (eopl:error 'apply-prim-proc "Incorrect type of 1st argument: ~s" prim-op)
                         (if (null? (cdr args))
                             (make-vector (1st args))
                             (make-vector (1st args) (2nd args))))]
      [(vector-ref) (vector-ref (1st args) (2nd args))];
      [(vector?) (vector? (1st args))];
      [(number?) (number? (1st args))];
      [(symbol?) (symbol? (1st args))];
      [(set-car!) (set-car! (1st args) (2nd args))]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
      [(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(display) (display (1st args))]
      [(newline) (newline)]
      
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
  (lambda (x) (top-level-eval (parse-exp x))))










