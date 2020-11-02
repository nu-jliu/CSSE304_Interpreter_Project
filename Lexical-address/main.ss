; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;                  
; Claude Anderson.  Last modified April, 2014January 2020.
; 
; Team names here: Allen Liu, Xingheng Lin, Steven Feng 

(load "chez-init.ss") 

(define load-all ; make it easy to reload the files
  (lambda ()
    (load "datatypes.ss")
    ;(load "parse-procs.ss")
    (load "syntax-expand.ss")
    (load "env-procs.ss")
    (load "continuations.ss")
    (load "parse-procs.ss")
    (load "interpreter.ss")
    (load "lexical-address.ss")))

(load-all)

(define l load-all) ; even easier!
