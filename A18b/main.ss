; Vanshika Reddy and Praneet Chakraborty

; evaluator for simple expressions.
; Possible starting point for first interpreter assignment.
;                  
; Claude Anderson.  Last modified April, 2014

(load "chez-init.ss") 

(define load-all ; make it easy to reload the files
	(lambda ()
		(load "datatypes.ss")
		(load "continuations.ss")
		(load "parse.ss")
		(load "env.ss")
		(load "syntax-expand.ss")
		(load "interpreter.ss")))

(load-all)

(define l load-all) ; even easier



; combined Vanshika's and Praneet's code for this assignment so this assignment is pretty different from A17