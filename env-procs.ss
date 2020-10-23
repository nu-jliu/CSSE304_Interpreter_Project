; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and  2.3

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (let loop ([los los] [pos 0])
      (cond [(null? los) #f]
	    [(eq? sym (car los)) pos]
	    [else (loop (cdr los) (add1 pos))]))))
	    
(define extend-env-recursively
  (lambda  (proc-names idss bodiess old-env)
    (let ([len (length proc-names)])
      (let ([vec (make-list len 0)])
        (let ([env (extended-env-record proc-names
                                        vec
                                        old-env)])
          (for-each (lambda (pos ids bodies)
                      (list-set vec
                                pos
                                (closure ids bodies env)))
                    (iota len)
                    idss
                    bodiess)
          env)))))

(define list-set
  (lambda (ls pos val)
    (if (zero? pos)
      (set-car! ls val)
      (list-set (cdr ls) (- pos 1) val))))

(define apply-env
  (lambda (env sym) 
    (cases environment env 
      [empty-env-record ()      
        (eopl:error 'env "variable ~s not found." sym)]
      [extended-env-record (syms vals env)
	(let ((pos (list-find-position sym syms)))
      	  (if (number? pos)
	      (list-ref vals pos)
	      (apply-env env sym)))])))

