; When you convert your interpreter to CPS for A18, this could be a good place
; to put your apply-k procedure and your continuation datatype definition.

(define-datatype continuation continuation?
  [ifelse-k 
    (ifdot expression?)
    (ifdof expression?)
    (env environment?)
    (k continuation?)]
  [if-k
    (ifdot expression?)
    (env environment?)
    (k continuation?)]
  [rator-k 
    (rands (list-of expression?))
    (env environment?)
    (k continuation?)]
  [rands-k 
    (proc-value scheme-value?)
    (k continuation?)]
  [env-k
    (bodies (list-of expression?))
    (k continuation?)]
  [global-env-k
    (k continuation?)]
  [app-env-k 
    (k continuation?)]
  [set!-k1
    (exp expression?)
    (env environment?)
    (k continuation?)]
  [set!-k2
    (cell cell?)
    (k continuation?)]
  [define-k
    (id symbol?)
    (k continuation?)]
  [eval-bodies-k
    (bodies (list-of expression?))
    (env environment?)
    (k continuation?)]
  [eval-rands-k1
    (car-rand expression?)
    (env environment?)
    (k continuation?)]
  [eval-rands-k2
    (cdr-rands (list-of scheme-value?))
    (k continuation?)]
  [map-k1
    (lst list?)
    (proc-value proc-val?)
    (k continuation?)]
  [map-k2
    (new-car scheme-value?)
    (k continuation?)]
  [proc-k
    (proc procedure?)]
  [init-k])

(define apply-k
  (lambda (k1 val)
    (cases continuation k1
      [ifelse-k (ifdot ifdof env k)
        (if val
          (eval-exp ifdot env k)
          (eval-exp ifdof env k))]
      [if-k (ifdot env k)
        (if val
          (eval-exp ifdot env k)
          (apply-k k (void)))]
      [rator-k (rands env k)
        (eval-rands rands
                    env
                    (rands-k val k))]
      [rands-k (proc-value k)
        (apply-proc proc-value val k)]
      [env-k (bodies k)
        (eval-bodies bodies val k)]
      [global-env-k (k)
        (apply-k k (set! global-env val))]
      [app-env-k (k)
        (apply-k k (cell-ref val))]
      [set!-k1 (exp env k)
        (eval-exp exp
                  env
                  (set!-k2 val k))]
      [set!-k2 (cell k)
        (apply-k k (cell-set! cell val))]
      [define-k (id k)
        (add-global-environment (list id)
                                (list val)
                                k)]
      [eval-bodies-k (bodies env k)
        (eval-bodies bodies env k)]
      [eval-rands-k1 (car-rands env k)
        (eval-exp car-rands
                  env
                  (eval-rands-k2 val k))]
      [eval-rands-k2 (cdr-rands k)
       (apply-k k (cons val cdr-rands))]
      [map-k1 (lst proc-value k)
        (map-proc-cps lst
                      proc-value
                      (map-k2 val k))]
      [map-k2 (new-car k)
        (apply-k k (cons new-car val))]
      [proc-k (proc)
        (proc val)]
      [init-k ()
        val])))

(define make-k
  (lambda (k)
    (proc-k k)))