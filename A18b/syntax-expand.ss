; Vanshika Reddy and Praneet Chakraborty

(define syntax-expand
	(lambda (exp)
		(cases expression exp
			[lit-exp (datum) (lit-exp datum)]
			[var-exp (datum) (var-exp datum)]
			[lambda-exp (vars body)
				(lambda-exp vars (map syntax-expand body))]
			[lambdavar-exp (idlist body)
				(lambdavar-exp idlist (map syntax-expand body))]
			[lambda-improper-exp (vars idlist body)
				(lambda-improper-exp vars idlist (map syntax-expand body))]
			[let-exp (vars values body)
				(app-exp (lambda-exp vars (map syntax-expand body)) (map syntax-expand values))]
			[let*-exp (vars values body)
				(app-exp (lambda-exp 
							(list (car vars))
							(if (null? (cdr vars))
								(map syntax-expand body)
								(list (syntax-expand (let*-exp (cdr vars) (cdr values) body)))))
					(list (syntax-expand (car values))))]
			[letrec-exp (names values body)
				(syntax-expand
					(let-exp
						names
						(map (lambda (x) (lit-exp #f)) names)
						(list (let ([temps (build-temps values)])
							(let-exp
								temps
								values
								(list (begin-exp
									(append
										(make-set!-list names temps)
										(list (let-exp '() '() body))))))))))]	
			[named-let-exp (name ids bodies named-let-bodies)
				(syntax-expand
					(app-exp
						(letrec-exp
							(list name)
							(list (lambda-exp ids named-let-bodies))
							(list (var-exp name)))
						bodies))]
			[if-exp (bool thencase)
				(if-exp 
					(syntax-expand bool)
					(syntax-expand thencase))]
			[if-else-exp (bool thencase elsecase)
				(if-else-exp
					(syntax-expand bool)
					(syntax-expand thencase)
					(syntax-expand elsecase))]
			[begin-exp (body)
				(app-exp (lambda-exp '() (map syntax-expand body)) '())]
			[set!-exp (var to-set-exp)
				(set!-exp var (syntax-expand to-set-exp))]
			[cond-exp (tests results)
				(if (null? (cdr tests))
					(if-exp
						(syntax-expand (1st tests))
						(app-exp (lambda-exp '() (map syntax-expand (1st results))) '()))
					(if-else-exp 
						(syntax-expand (1st tests))
						(app-exp (lambda-exp '() (map syntax-expand (1st results))) '())
						(syntax-expand (cond-exp (cdr tests) (cdr results)))))]
			[and-exp (body)
				(if (null? (cdr body))
					(if-else-exp
						(syntax-expand (1st body))
						(syntax-expand (1st body))
						(lit-exp #f))
					(if-else-exp
						(syntax-expand (1st body))
						(syntax-expand (and-exp (cdr body)))
						(lit-exp #f)))]
			[or-exp (body)
				(app-exp
					(lambda-exp 
						(list 'if-else-test)
						(list
							(if-else-exp
								(var-exp 'if-else-test)
								(var-exp 'if-else-test)
								(if (null? (cdr body))
									(lit-exp #f)
									(syntax-expand (or-exp (cdr body)))))))
					(list (syntax-expand (car body))))]
			[case-exp (id xcase bodies)
				(if (null? (cdr xcase))
					(if-exp (case-or-expression id (1st xcase))
						(syntax-expand (1st bodies)))
					(if-else-exp (case-or-expression id (1st xcase))
						(syntax-expand (1st bodies))
						(syntax-expand (case-exp id (cdr xcase) (cdr bodies)))))]
			[while-exp (checker body)
				(app-exp (lambda-exp (list 'x) (list (app-exp (var-exp 'x) (list (var-exp 'x)))))
					(list (lambda-exp (list 'y) (list (if-exp
						(syntax-expand checker)
						(app-exp (lambda-exp '()
							(append body
									(list (app-exp (var-exp 'y) (list (var-exp 'y))))))
							'()))))))]
			[define-exp (id exp)
				(define-exp id (syntax-expand exp))]
			[app-exp (rator rands)
				(app-exp
					(syntax-expand rator)
					(map syntax-expand rands))]
			[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))