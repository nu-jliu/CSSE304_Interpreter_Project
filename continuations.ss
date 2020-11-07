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
  [proc-k
    (proc procedure?)]
  [init-k])

(define apply-k
  (lambda (k val)
    (cases continuation k
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
      [proc-k (proc)
        (proc val)]
      [init-k ()
        val])))

(define make-k
  (lambda (k)
    (proc-k k)))