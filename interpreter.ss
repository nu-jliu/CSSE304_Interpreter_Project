; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env (init-k))))

; eval-exp is the main component of the interpreter

    
(define eval-exp
  (lambda (exp env k)
    (cases expression exp
      [lit-exp (datum) (apply-k k datum)]
      [var-exp (id) 
        (apply-env env id k)]
      [lambda-exp (id body)
        (apply-k k (closure id body env))]
      [lambda-x-exp (ids bodies)
        (apply-k k (closure ids bodies env))]
      [lambdaImp-exp (ids bodies)
        (apply-k k (closure ids bodies env))]
      [case-lambda-exp (idss bodiess)
        (apply-k k (closure-list idss bodiess env))]
      [if-exp (ifpred ifdot)
        ;(if (eval-exp ifpred env) 
        ;  (eval-exp ifdot env))
        (eval-exp ifpred
                  env
                  (if-k ifdot env k))]
      [ifelse-exp (ifpred ifdot ifdof)
        (eval-exp ifpred
                  env
                  (ifelse-k ifdot ifdof env k))]
        ;(if (eval-exp ifpred env) 
        ;  (eval-exp ifdot env) 
        ;  (eval-exp ifdof env))
        
      [app-exp (rator rands)
        ; (let ([proc-value (eval-exp rator env)]
        ;       [args (eval-rands rands env)])
        ;   (apply-proc proc-value args))
        (eval-exp rator 
                  env
                  (rator-k rands env k))]
      [while-exp (test-exp bodies)
        (let loop ([t test-exp])
          (if (eval-exp t env)
            (begin (eval-bodies bodies env) 
                   (loop t))
            (eval-exp (app-exp (var-exp 'void) '()) env)))] ; to be completed later (\^_^/)
      [letrec-exp (proc-names idss bodiess letrec-bodies)
        ;(eval-bodies letrec-bodies 
                     (extend-env-recursively proc-names 
                                             idss bodiess 
                                             env ;; non-CPSed code above ($_$)
                                             ;(make-k (lambda (extended-env)
                                             ;          (eval-bodies letrec-bodies
                                             ;                       extended-env
                                             ;                       k)))
                                             (env-k letrec-bodies k))]
      [set!-exp (id val-exp)
        ;(cell-set! 
        ;           (apply-env-ref env id)
        ;            (eval-exp exp env))
        (apply-env-ref env
                       id
                       ;(make-k (lambda (cell)
                       ;          (eval-exp exp
                       ;                    env
                       ;                    (make-k (lambda (value)
                       ;                              (apply-k k (cell-set! cell value)))))))
                       (set!-k1 val-exp env k))]
      [define-exp (id val-exp)
        ;(if (contains-env env id)
          ; (error 'eval-exp
          ;        "~a is already in environment"
          ;        id)
          ;(add-global-environment (list id) 
          ;                        (list (eval-exp val env k)))
          (eval-exp val-exp
                    env
                    ;(make-k (lambda (new-val)
                    ;          (add-global-environment (list id)
                    ;                                  (list new-val)
                    ;                                  k)))
                    (define-k id k))]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define eval-bodies
  (lambda (bodies env)
    (if (null? (cdr bodies))
      (eval-exp (car bodies) env)
      (begin (eval-exp (car bodies) env)
        (eval-bodies (cdr bodies) env)))))

(define eval-bodies
  (lambda (bodies env k)
    (if (null? (cdr bodies))
      (eval-exp (car bodies) env k)
      (eval-exp (car bodies)
                env
                ;(make-k (lambda (car-bodies)
                ;          (eval-bodies (cdr bodies) env k)))
                (eval-bodies-k (cdr bodies) env k)))))

;(define eval-bodies
;  (lambda (body env)
;    (if (null? (cdr body))
;        (eval-exp (car body) env)
;        (begin
;          (eval-exp (car body) env)
;          (eval-bodies (cdr body) env)))))

(define eval-rands
  (lambda (rands env k)
    (if (null? rands)
      (apply-k k '())
      (eval-rands (cdr rands)
                  env
                  ;(make-k (lambda (cdr-rands)
                  ;          (eval-exp (car rands)
                  ;                    env
                  ;                    (make-k (lambda (car-rands)
                  ;                      (apply-k k (cons car-rands cdr-rands)))))))
                  (eval-rands-k1 (car rands) env k)))))

;(define map-cps
;  (lambda (proc-cps L K)
 ;   (if (null? L)
 ;     (apply-k K '())
;      (map-cps proc-cps 
;               (cdr L) 
;               (make-k (lambda (cdr-L)
;                         (proc-cps (car L) 
;                                   (make-k (lambda (r-L)
;                                             (apply-k K (cons r-L cdr-L)))))))))))

(define map-proc-cps
        (lambda (lst proc-value k)
          (if (null? lst) 
            (apply-k k '())
            (apply-proc proc-value
                        (list (car lst))
                        ;(make-k (lambda (new-car)
                        ;          (map-proc-cps (cdr lst)
                        ;                        proc-value
                        ;                        (make-k (lambda (cdr-map)
                        ;                                  (apply-k k (cons new-car cdr-map)))))))
                        (map-k1 (cdr lst) proc-value k)))))
;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
      [prim-proc (op) 
        (apply-prim-proc op args k)]
      [closure (id bodies env) 
        (cond [(symbol? id) ;(eval-bodies bodies 
                            ;             (extend-env (list id) 
                            ;                         (list args) 
                            ;                         env))
                (extend-env (list id)
                            (list args)
                            env
                            ;(make-k (lambda (extended-env)
                            ;          (eval-bodies bodies extended-env k)))
                            (env-k bodies k))]
              [((list-of symbol?) id) 
                ;(eval-bodies body 
                ;            (extend-env id args env))
                (extend-env id
                            args
                            env
                            ;(make-k (lambda (extended-env)
                            ;          (eval-bodies bodies extended-env k)))
                            (env-k bodies k))]
              [(pair? id) ;(eval-bodies body 
                          ;             (extend-env (car (imhelper id args)) 
                          ;                         (cadr (imhelper id args)) 
                          ;                         env))
                          (let ([id-args (imhelper id args)])
                            (extend-env (car id-args)
                                        (cadr id-args)
                                        env
                                        ;(make-k (lambda (extended-env)
                                        ;          (eval-bodies bodies extended-env k)))
                                        (env-k bodies k)))])]
      [closure-list (idss bodiess env)
        (let closure-helper ([idss idss]
                             [bodiess bodiess])
          (cond [(null? idss) (error 'apply-proc
                                     "Incorrect argument format ~s"
                                     proc-value)]
                [(list? (car idss)) (if (eqv? (length args) 
                                              (length (car idss)))
                                      (apply-proc (closure (car idss) 
                                                           (car bodiess) 
                                                           env)
                                                  args)
                                      (closure-helper (cdr idss) (cdr bodiess)))]
                [(pair? (car idss)) (if (null? args)
                                        (closure-helper (cdr idss) (cdr bodiess))
                                        (apply-proc (closure (car idss) 
                                                             (car bodiess) 
                                                             env)
                                                    args))]
                [(symbol? (car idss)) (apply-proc (closure (car idss)
                                                           (car bodiess)
                                                           env)
                                                  args)]
                [else (closure-helper (cdr idss) (cdr bodiess))]))]  
      [k-proc (k)
        (apply-k k (car args))]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define imhelper
    (lambda (ids args)
      (if (symbol? ids)
        (list (list ids) (list args))
        (let ([id-list (car (imhelper (cdr ids) (cdr args)))]
              [arg-list (cadr (imhelper (cdr ids) (cdr args)))])
          (list (cons (car ids) id-list) 
                (cons (car args) arg-list))))))


(define *prim-proc-names* '(+ - * add1 sub1 cons = / zero? not < > <= >= car cdr caar cadr cdar cddr 
                            caaar caadr cadar caddr cdaar cdadr cddar cdddr list null? assq eq? eqv?
                            equal? atom? length list->vector list? pair? procedure? vector->list vector
                            make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set!
                            map list apply memq void quotient append list-tail;A14
                            display newline call/cc exit-list))

(define global-env
  (extend-env *prim-proc-names*           ; procedure names.  Recall that an environment associates
              (map prim-proc                        ;  a value (not an expression) with an identifier.
                   *prim-proc-names*)
              (empty-env)
              (init-k)))

(define reset-global-env
  (lambda ()
  (set! global-env (extend-env *prim-proc-names*           ; procedure names.  Recall that an environment associates
              (map prim-proc                        ;  a value (not an expression) with an identifier.
                   *prim-proc-names*)
              (empty-env)
              (init-k)))))

(define init-env         ; for now, our initial global environment only contains 
 (empty-env))
; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args k)
    (case prim-proc
      [(+) (apply-k k (apply + args))]
      [(-) (apply-k k (apply - args))]
      [(*) (apply-k k (apply * args))]
      [(/) (apply-k k (apply / args))]
      [(add1) (apply-k k (+ (1st args) 1))]
      [(sub1) (apply-k k (- (1st args) 1))]
      [(cons) (apply-k k (cons (1st args) (2nd args)))]
      [(=) (apply-k k (= (1st args) (2nd args)))]
      [(zero?) (apply-k k (if (null? (cdr args))
                            (zero? (1st args))
                            (eopl:error 'apply-prim-proc 
                                        "Incorrect number of arguments: ~s" 
                                        prim-op)))]
      [(not) (apply-k k (not (1st args)))];
      [(<) (apply-k k (apply < args))]
      [(>) (apply-k k (apply > args))]
      [(<=) (apply-k k (apply <= args))]
      [(>=) (apply-k k (apply >= args))]
      [(car) (apply-k k (car (1st args)))];
      [(cdr) (apply-k k (cdr (1st args)))];
      [(caar) (apply-k k (caar (1st args)))];
      [(cadr) (apply-k k (cadr (1st args)))];
      [(cdar) (apply-k k (cdar (1st args)))];
      [(cddr) (apply-k k (cddr (1st args)))];
      [(caaar) (apply-k k (caaar (1st args)))];
      [(caadr) (apply-k k (caadr (1st args)))];
      [(cadar) (apply-k k (cadar (1st args)))];
      [(caddr) (apply-k k (caddr (1st args)))];
      [(cdaar) (apply-k k (cdaar (1st args)))];
      [(cdadr) (apply-k k (cdadr (1st args)))];
      [(cddar) (apply-k k (cddar (1st args)))];
      [(cdddr) (apply-k k (cdddr (1st args)))];
      [(list) (apply-k k args)]
      [(null?) (apply-k k (null? (1st args)))];
      [(map) (map-proc-cps (cadr args)
                           (1st args)
                           k)] ;(apply map 
                        ;      (lambda (x) 
                        ;        (apply-proc (1st args) (list x) k))
                        ;      (cdr args)))
                        ;A14

      

      [(apply) (apply (lambda (x) 
                        (apply-proc (1st args) 
                                    x 
                                    k)) 
                      (cdr args))];A14
      [(assq) (apply-k k (assq (1st args) (2nd args)))];
      [(eq?) (apply-k k (eq? (1st args) (2nd args)))];
      [(eqv?) (apply-k k (eqv? (1st args) (2nd args)))];
      [(equal?) (apply-k k (equal? (1st args) (2nd args)))];
      [(atom?) (apply-k k (atom? (1st args)))];
      [(length) (apply-k k (length (1st args)))];
      [(list->vector) (apply-k k (list->vector (1st args)))];
      [(list?) (apply-k k (list? (1st args)))];
      [(list-tail) (apply-k k (list-tail (1st args) (2nd args)))];
      [(pair?) (apply-k k (pair? (1st args)))];
      [(procedure?) (apply-k k (proc-val? (1st args)))];
      [(vector->list) (apply-k k (vector->list (1st args)))];
      [(vector) (apply-k k (apply vector args))]
      [(make-vector) (apply-k k (if (not (number? (1st args)))
                         (eopl:error 'apply-prim-proc 
                                     "Incorrect type of 1st argument: ~s" 
                                     prim-op)
                         (if (null? (cdr args))
                           (make-vector (1st args))
                           (make-vector (1st args) (2nd args)))))]
      [(vector-ref) (apply-k k (vector-ref (1st args) (2nd args)))];
      [(vector?) (apply-k k (vector? (1st args)))];
      [(number?) (apply-k k (number? (1st args)))];
      [(symbol?) (apply-k k (symbol? (1st args)))];
      [(set-car!) (apply-k k (set-car! (1st args) (2nd args)))]
      [(set-cdr!) (apply-k k (set-cdr! (1st args) (2nd args)))]
      [(vector-set!) (apply-k k (vector-set! (1st args) (2nd args) (3rd args)))]
      [(display) (display (1st args))]
      [(newline) (newline)]
      [(memq) (apply-k k (memq (1st args) (2nd args)))]
      [(void) (apply-k k (void))]
      [(quotient) (apply-k k (quotient (1st args) (2nd args)))]
      [(append) (apply-k k (append (1st args) (2nd args)))]
      [(call/cc) (apply-proc (car args)
                             (list (k-proc k))
                             k)]
      [(exit-list) (apply-prim-proc 'list
                                    args
                                    (init-k))]
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
  (lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))