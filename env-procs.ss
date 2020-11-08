; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and  2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env k)
    (apply-k k (extended-env-record syms 
                                    (map cell vals) 
                                    env))))

(define list-find-position
  (lambda (sym los)
    (let loop ([los los] [pos 0])
      (cond [(null? los) #f]
	    [(eq? sym (car los)) pos]
	    [else (loop (cdr los) (add1 pos))]))))
	    
(define extend-env-recursively
  (lambda  (proc-names idss bodiess old-env k)
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
          (apply-k k env))))))

(define add-global-environment
  (lambda (syms vals k)
    ;(set! global-env (extend-env syms vals global-env))
    (extend-env syms
                vals
                global-env
                ;(make-k (lambda (extended-env)
                ;          (apply-k k (set! global-env extended-env))))
                (global-env-k k))))

(define list-set
  (lambda (ls pos val)
    (if (zero? pos)
      (set-car! ls val)
      (list-set (cdr ls) (- pos 1) val))))

(define apply-env-ref ;return the reference
  (lambda (env sym k) 
    (cases environment env 
      [empty-env-record ()      
        (apply-env-c global-env sym k)]
      [extended-env-record (syms vals env)
	      (let ([pos (list-find-position sym syms)])
      	  (if (number? pos)
	          (apply-k k (list-ref vals pos))
	          (apply-env-ref env sym k)))])))

(define apply-env 
  (lambda (env var k)
    (apply-env-ref env 
                   var
                   ;(make-k (lambda (cell)
                   ;          (apply-k k (cell-ref cell))))
                   (app-env-k k))));take the value from the reference 

(define apply-env-c
  (lambda (env sym k) 
    (cases environment env 
      [empty-env-record ()      
        (error 'env "variable ~s not found" sym)]
      [extended-env-record (syms vals env)
	      (let ([pos (list-find-position sym syms)])
      	  (if (number? pos)
	          (apply-k k (list-ref vals pos))
	          (apply-env-c env sym k)))])))

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

