; When you write syntax-expand for A14, this could be a good place to pt your code.

(define syntax-expand 
	(lambda (exp)
	(cases expression exp
		[cond-exp  (body) 
			         (syntax-expand)]
	    [or-exp    (body) 

	           ]
	    [and-exp   (body) 
	           ]
	    [case-exp  (body) 
	           ]
	    [begin-exp (body) 
	    	
	           ]      

	    [else exp]