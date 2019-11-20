; Vanshika Reddy and Praneet Chakraborty


(load "chez-init.ss") ; put this file in the same folder, or add a pathname

; This is a parser for simple Scheme expressions, 
; such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.


; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define parse-exp         
	(lambda (datum)
		(cond
			[(symbol? datum) (var-exp datum)]
			[(or (number? datum) (vector? datum) (boolean? datum) (symbol? datum) (string? datum) (char? datum)) (lit-exp datum)]
			[(list? datum)
				(cond
					[(eqv? (1st datum) 'quote)
						(lit-exp (2nd datum))]
					[(eqv? (1st datum) 'lambda)
						(if (null? (cddr datum))
							(eopl:error 'parse-exp "lambda-exp: incorrect length: ~s" datum)
							(cond
								[(symbol? (2nd datum))
									(lambdavar-exp
										(2nd datum)
										(map parse-exp (cddr datum)))]
								[(list? (2nd datum))
									(if (andmap symbol? (2nd datum))
										(lambda-exp (2nd datum)
											(map parse-exp (cddr datum)))
										(eopl:error 'parse-exp "lambda-exp: formal arguments must be symbols: ~s" datum))]
								[else (let ([improper (improper-lambda-helper (2nd datum))])
									(lambda-improper-exp
										(car improper)
										(cadr improper)
										(map parse-exp (cddr datum))))]))]
					[(eqv? (1st datum) 'let)
						(cond
							[(or (null? (cdr datum)) (null? (cddr datum)))
								(eopl:error 'parse-exp "let-exp: incorrect length: ~s" datum)]
							[(or (pair? (2nd datum)) (null? (2nd datum)))
								(cond
									[(not (list? (2nd datum)))
										(eopl:error 'parse-exp "let-exp: declarations not in list: ~s" datum)]
									[(not (andmap list? (2nd datum)))
										(eopl:error 'parse-exp "let-exp: declarations are not a list: ~s" datum)]
									[(not (andmap let-def-symbol? (2nd datum)))
										(eopl:error 'parse-exp "let-exp: vars must be symbols: ~s" datum)]
									[(ormap let-def-invalid-length? (2nd datum))
										(eopl:error 'parse-exp "let-exp: declaration must be a list of length 2: ~s" datum)]
									[else
										(let-exp
											(map car (2nd datum))
											(map let-symbols (2nd datum))
											(map parse-exp (cddr datum)))])]
							[else
								(cond
									[(not (list? (3rd datum)))
										(eopl:error 'parse-exp "let-named-exp: declarations not in list: ~s" datum)]
									[(not (andmap list? (3rd datum)))
										(eopl:error 'parse-exp "let-named-exp: declarations are not a list: ~s" datum)]
									[(not (andmap let-def-symbol? (3rd datum)))
										(eopl:error 'parse-exp "let-named-exp: vars must be symbols: ~s" datum)]
									[(ormap let-def-invalid-length? (3rd datum))
										(eopl:error 'parse-exp "let-named-exp: declaration must be a list of length 2: ~s" datum)]
									[else
										(named-let-exp
											(2nd datum)
											(map car (3rd datum))
											(map let-symbols (3rd datum))
											(map parse-exp (cdddr datum)))])])]
					[(eqv? (1st datum) 'let*)
						(cond
							[(or (null? (cdr datum)) (null? (cddr datum)))
								(eopl:error 'parse-exp "let*-exp: incorrect length: ~s" datum)]
							[(not (list? (2nd datum)))
								(eopl:error 'parse-exp "let*-exp: declarations not in list: ~s" datum)]
							[(not (andmap list? (2nd datum)))
								(eopl:error 'parse-exp "let*-exp: declarations are not a list: ~s" datum)]
							[(not (andmap let-def-symbol? (2nd datum)))
								(eopl:error 'parse-exp "let*-exp: vars must be symbols: ~s" datum)]
							[(ormap let-def-invalid-length? (2nd datum))
								(eopl:error 'parse-exp "let*-exp: declaration must be a list of length 2: ~s" datum)]
							[else
								(let*-exp 
									(map car (2nd datum))
									(map let-symbols (2nd datum))
									(map parse-exp (cddr datum)))])]
					[(eqv? (1st datum) 'letrec)
						(cond
							[(or (null? (cdr datum)) (null? (cddr datum)))
								(eopl:error 'parse-exp "letrec-exp: incorrect length: ~s" datum)]
							[(not (list? (2nd datum)))
								(eopl:error 'parse-exp "letrec-exp: declarations not in list: ~s" datum)]
							[(not (andmap list? (2nd datum)))
								(eopl:error 'parse-exp "letrec-exp: declarations are not a list: ~s" datum)]
							[(not (andmap let-def-symbol? (2nd datum)))
								(eopl:error 'parse-exp "letrec-exp: vars must be symbols: ~s" datum)]
							[(ormap let-def-invalid-length? (2nd datum))
								(eopl:error 'parse-exp "letrec-exp: declaration must be a list of length 2: ~s" datum)]
							[else
								(letrec-exp 
									(map car (2nd datum))
									(map let-symbols (2nd datum))
									(map parse-exp (cddr datum)))])]
					[(eqv? (1st datum) 'if)
						(cond
							[(or (null? (cdr datum)) (null? (cddr datum)))
								(eopl:error 'parse-exp "if-exp: invalid length: ~s" datum)]
							[(null? (cdddr datum))
								(if-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
							[(not (null? (cddddr datum)))
								(eopl:error 'parse-exp "if-exp: invalid length: ~s" datum)]
							[else (if-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)) (parse-exp (4th datum)))])]
					[(eqv? (1st datum) 'begin)
						(begin-exp (map parse-exp (cdr datum)))]
					[(eqv? (1st datum) 'set!)
						(if (or (null? (cdr datum)) (null? (cddr datum)) (not (null? (cdddr datum))))
							(eopl:error 'parse-exp "set!-exp: invalid length: ~s" datum)
							(set!-exp (cadr datum)
								(parse-exp (3rd datum))))]
					[(eqv? (1st datum) 'cond)
						(cond-exp 
							(map (lambda (x) 
									(if (eqv? (car x) 'else)
										(lit-exp #t)
										(parse-exp (car x))))
								(cdr datum))
							(map (lambda (x) (map parse-exp (cdr x))) (cdr datum)))]
					[(eqv? (1st datum) 'and)
						(if (null? (cdr datum))
							(lit-exp #t)
							(and-exp (map parse-exp (cdr datum))))]
					[(eqv? (1st datum) 'or)
						(if (null? (cdr datum))
							(lit-exp #f)
							(or-exp (map parse-exp (cdr datum))))]
					[(eqv? (1st datum) 'case)
						(case-exp (parse-exp (2nd datum))
							(map (lambda (x)
									(if (eqv? (car x) 'else)
										(list (parse-exp (2nd datum)))
										(map parse-exp (1st x))))
								(cddr datum))
							(map (lambda (x) (begin-exp (map parse-exp (cdr x)))) (cddr datum)))]
					[(eqv? (1st datum) 'while)
						(while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))]
					[(eqv? (1st datum) 'define)
						(define-exp (2nd datum) (parse-exp (3rd datum)))]
					[else 
						(app-exp 
							(parse-exp (1st datum))
							(map parse-exp (cdr datum)))])]
			[else (eopl:error 'parse-exp "expression: ~s is not a proper list:" datum)])))

