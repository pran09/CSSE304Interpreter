; Vanshika Reddy and Praneet Chakraborty


; Cell

(define cell
	(lambda (val)
		(cons val 'cell)))

(define cell-ref car)

(define set-cell! set-car!)

(define cell?
	(lambda (obj)
		(and
			(pair? obj)
			(eqv? (cdr obj) 'cell))))

; Parsed expression datatypes

(define-datatype expression expression?
	[lit-exp
		(datum 
			(lambda (x)
				(or
				 (boolean? x) (number? x) (string? x) (symbol? x) (vector? x) (null? x) (pair? x) (char? x))))]
	[var-exp
		(id symbol?)]
	[lambda-exp
		(datum (list-of symbol?))
		(body (list-of expression?))]
	[lambdavar-exp
		(datum symbol?)
		(body (list-of expression?))]
	[lambda-improper-exp
		(datum (list-of symbol?))
		(idlist symbol?)
		(body (list-of expression?))]
	[if-exp
		(bool expression?)
		(thencase expression?)]
	[if-else-exp
		(bool expression?)
		(thencase expression?)
		(elsecase expression?)]
	[let-exp
		(vars (list-of symbol?))
		(values (list-of expression?))
		(body (list-of expression?))]
	[let*-exp
		(vars (list-of symbol?))
		(values (list-of expression?))
		(body (list-of expression?))]
	[letrec-exp
		(names (list-of symbol?))
		(values (list-of expression?))
		(body (list-of expression?))]
	[named-let-exp
		(name symbol?)
		(ids (list-of symbol?))
		(bodies (list-of expression?))
		(named-let-bodies (list-of expression?))]
	[set!-exp
		(var symbol?)
		(to-set-exp expression?)]
	[begin-exp
		(body (list-of expression?))]
	[define-exp
		(var symbol?)
		(body expression?)]
	[cond-exp
		(tests (list-of expression?))
		(results (list-of (list-of expression?)))]
	[and-exp
		(body (list-of expression?))]
	[or-exp
		(body (list-of expression?))]
	[case-exp
		(id expression?)
		(xcase (list-of (list-of expression?)))
		(bodies (list-of expression?))]
	[while-exp
		(checker expression?)
		(body (list-of expression?))]
	[app-exp
		(rator expression?)
		(rands (list-of expression?))])

(define let-symbols
	(lambda (x)
		(parse-exp (cadr x))))

(define let-def-invalid-length?
	(lambda (x)
		(or (null? (cdr x)) (not (null? (cddr x))))))

(define let-def-symbol?
	(lambda (x)
		(symbol? (car x))))

(define improper-lambda-helper
	(lambda (ils)
		(if (pair? (cdr ils))
			(let ([var (improper-lambda-helper (cdr ils))])
				(list
					(cons (car ils) (car var))
					(cadr var)))
			(list (list (car ils)) (cdr ils)))))

;; environment type definitions

(define scheme-value?
	(lambda (x) #t))

(define-datatype environment environment?
	[empty-env-record]
	[extended-env-record
		(syms (list-of symbol?))
		(vals (list-of cell?))
		(env environment?)])
	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
	[continuation-proc
		(k continuation?)]
	[prim-proc
		(name symbol?)]
	[closure
		(params (list-of symbol?))
		(bodies (list-of expression?))
		(env environment?)]
	[closure-var
		(ids symbol?)
		(bodies (list-of expression?))
		(env environment?)]
	[closure-improper
		(params (list-of symbol?))
		(listsymbol symbol?)
		(bodies (list-of expression?))
		(env environment?)])
	 
