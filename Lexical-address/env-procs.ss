; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and  2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (map cell vals) env)))

(define list-find-position
  (lambda (sym los)
    (let loop ([los los] [pos 0])
      (cond [(null? los) #f]
	    [(eq? sym (car los)) pos]
	    [else (loop (cdr los) (add1 pos))]))))
	    
(define extend-env-recursively
  (lambda  (proc-names idss bodiess old-env)
    (let ([len (length proc-names)])
      (let ([vec (make-list len (cell 0))])
        (let ([env (extended-env-record proc-names
                                         vec
                                        old-env)])
          (for-each (lambda (pos ids bodies)
                      (list-set vec
                                pos
                                 (cell (closure ids bodies env))))
                    (iota len)
                    idss
                    bodiess)
          env)))))

(define add-global-environment
  (lambda (syms vals)
    (set! global-env (extend-env syms vals global-env))))

(define list-set
  (lambda (ls pos val)
    (if (zero? pos)
      (set-car! ls val)
      (list-set (cdr ls) (- pos 1) val))))

(define apply-env-ref ;return the reference
  (lambda (env sym) 
    (cases environment env 
      [empty-env-record ()      
        (apply-env-c global-env sym)]
      [extended-env-record (syms vals env)
	      (let ([pos (list-find-position sym syms)])
      	  (if (number? pos)
	          (list-ref vals pos)
	          (apply-env-ref env sym)))])))

(define apply-env 
  (lambda (env var)
    (cell-ref (apply-env-ref env var))));take the value from the reference 

(define apply-env-c
  (lambda (env sym) 
    (cases environment env 
      [empty-env-record ()      
        (error 'env "variable ~s not found" sym)]
      [extended-env-record (syms vals env)
	      (let ([pos (list-find-position sym syms)])
      	  (if (number? pos)
	          (list-ref vals pos)
	          (apply-env-c env sym)))])))

(define contains-env
  (lambda (env sym) 
    (cases environment env 
      [empty-env-record ()      
        #f]
      [extended-env-record (syms vals env)
	      (let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	        #t
	        (contains-env env sym)))])))

