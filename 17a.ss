; Authors: Praneet Chakraborty, Vanshika Reddy (rip neelie and amruth)

(load "chez-init.ss") 
; (load "C:\\Users\\shahnv\\Documents\\School Files\\Y2\\Q1\\PLC\\CSSE304\\Project\\chez-init.ss")


;-------------------+
;                   |
;     DATATYPES     |
;                   |
;-------------------+

; parsed expression

(define-datatype expression expression?  
	[var-exp        			; variable references
  		(id symbol?)]
  	[lit-exp        			; "Normal" data
  		(datum
  			(lambda (x)
  				(ormap 
  					(lambda (pred) (pred x))
  					(list number? vector? boolean? symbol? string? pair? null?))))]
  	[app-exp        			; applications
  		(rator expression?)
  		(rands (list-of expression?))]
  	[case-lambda-exp
		(params (list-of (lambda (x) (or (list-of symbol? x) (symbol? x)))))
  		(bodies (list-of (list-of expression?)))]
  	[let-exp					; let
  		(vars (list-of symbol?))
  		(exps (list-of expression?))
  		(bodies (list-of expression?))]
  	[lambda-exp					; lambda
  		(vars (list-of symbol?))
  		(bodies (list-of expression?))]
  	[improper-lambda-exp		; improper params lambda
  		(vars (list-of symbol?))
  		(bodies (list-of expression?))]
  	[if-exp 					; if
  		(test-exp expression?)
  		(then-exp expression?)
  		(else-exp expression?)]
  	[if-no-else-exp				; one armed if
  		(test-exp expression?)
  		(then-exp expression?)]
  	[while-exp					; while (not originally in Scheme)
  		(test-exp expression?)
  		(bodies (list-of expression?))]
  	[let*-exp
  		(vars (list-of symbol?))
  		(exps (list-of expression?))
  		(bodies (list-of expression?))]
  	[named-let-exp
  		(name symbol?)
  		(ids (list-of symbol?))
  		(bodies (list-of expression?))
  		(named-let-bodies (list-of expression?))]
	[letrec-exp
		(proc-names (list-of symbol?))
		(ids (list-of (lambda (x) (or (list-of symbol? x) (pair? x)))))
		(bodies (list-of (list-of expression?)))
		(letrec-bodies (list-of expression?))]
	[and-exp
        (body (list-of expression?))]
    [or-exp
        (body (list-of expression?))]
  	[case-exp
  		(test-exp expression?)
  		(compare-exp (list-of (lambda (x) (or (list? x) (equal? 'else x)))))
  		(outcome-exp (list-of expression?))]
	[define-exp
		(var symbol?)
		(body-exp expression?)])


; cell type definitions
 
(define cell
	(lambda (val)
		(box val)))
		
(define cell-ref
	(lambda (cell)
		(unbox cell)))
		
(define cell? box?)

(define cell-set!
	(lambda (cell val)
		(set-box! cell val)))


;; environment type definitions

(define scheme-value?
	(lambda (x) #t))

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype environment environment?
	[empty-env-record]
	[extended-env-record
		(syms (list-of symbol?))
		(vals (list-of scheme-value?))
		(env environment?)]
	[recursively-extended-env-record
		(proc-names (list-of symbol?))
		(idss (list-of (lambda (x) (or (list-of symbol? x) (pair? x)))))
		(bodies (list-of (list-of expression?)))
		(env environment?)])



(define-datatype proc-val proc-val?
	[prim-proc 			; primitive
		(name symbol?)]
	[closure 			; defined closure (lambda)
		(vars (list-of symbol?))
		(bodies (list-of expression?))
		(env environment?)]
	[improper-closure 	; closure for improper lambda
		(vars (list-of symbol?))
		(bodies (list-of expression?))
		(env environment?)]
	[case-closure		; closure for case-lambda
		(params (list-of (lambda (x) (or (list-of symbol? x) (symbol? x)))))
		(bodies (list-of (list-of expression?)))
		(env environment?)])


;-------------------+
;                   |
;      PARSER       |
;                   |
;-------------------+


; This is a parser for simple Scheme expressions, such as those in EOPL, 3.1 thru 3.3.

; You will want to replace this with your parser that includes more expression types, more options for these types, and error-checking.

; Procedures to make the parser a little bit saner.
(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)

(define list-improper?
	(lambda (ls)
		(cond
			[(null? ls) #f]
			[(list? (car ls)) (list-improper? (cdr ls))]
			[else #t])))
			
(define list-pair?
	(lambda (ls)
		(cond
			[(null? ls) #t]
			[(null? (cdar ls)) #f]
			[(not (null? (cddar ls))) #f]
			[else (list-pair? (cdr ls))])))
			
(define list-all-symbols?
	(lambda (ls)
		(cond
			[(null? ls) #t]
			[(symbol? (caar ls)) (list-all-symbols? (cdr ls))]
			[else #f])))

(define pair-firsts
	(lambda (ls)
		(cond
			[(null? ls) '()]
			[else (cons (caar ls) (pair-firsts (cdr ls)))])))

(define pair-seconds
	(lambda (ls)
		(cond
			[(null? ls) '()]
			[else (cons (cadar ls) (pair-seconds (cdr ls)))])))

(define pair-cddrs
	(lambda (ls)
		(cond
			[(null? ls) '()]
			[else (cons (cddar ls) (pair-cddrs (cdr ls)))])))
				
(define pair-construct
	(lambda (first second)
		(cond 
			[(or (null? first) (null? second)) '()]
			[else (cons (list (car first) (car second)) (pair-construct (cdr first) (cdr second)))])))

(define improper-list->list
	(lambda (ls)
		(cond
			[(list? ls) ls]
			[(not (pair? ls)) (list ls)]
			[else (cons (car ls) (improper-list->list (cdr ls)))])))

(define map-parse-exp
	(lambda (exp-ls)
		(cond
			[(null? exp-ls) '()]
			[else (cons (map parse-exp (car exp-ls)) (map-parse-exp (cdr exp-ls)))])))

(define parse-exp-case-lambda
	(lambda (datum)
		(cond
			[(null? datum) '()]
			[else (cons (map parse-exp (cdr (1st datum))) (parse-exp-case-lambda (cdr datum)))])))

(define parse-exp         
	(lambda (datum)
		(cond
			[(symbol? datum) (var-exp datum)]
			[(or (number? datum) (vector? datum) (boolean? datum) (symbol? datum) (string? datum)) (lit-exp datum)]
			[(eqv? 'quote (car datum)) (lit-exp (cdr datum))]
			[(pair? datum)
			(cond
				[(eqv? (car datum) 'case-lambda)
					(case-lambda-exp (map 1st (cdr datum)) (parse-exp-case-lambda (cdr datum)))]
				[(eqv? (car datum) 'if)
					(cond
						[(or (null? (cdr datum)) (null? (cddr datum)))
							(eopl:error 'parse-exp "if-exp ~s does not contain correct amount of expressions" datum)]
						[(null? (cdddr datum)) 
							(if-no-else-exp (parse-exp (2nd datum)) (parse-exp (3rd datum)))]
						[(not (null? (cddddr datum))) 
							(eopl:error 'parse-exp "if-exp ~s has too many expressions" datum)]
						[else (if-exp (parse-exp (2nd datum)) 
								(parse-exp (3rd datum)) (parse-exp (4th datum)))])]
				[(eqv? (car datum) 'let)
					(cond
						[(null? (cddr datum)) 
							(eopl:error 'parse-exp "~s-expression has no body ~s" datum)]
						[(and (symbol? (2nd datum)) (andmap symbol? (map 1st (3rd datum))))
                                (named-let-exp (2nd datum) (map 1st (3rd datum)) (map parse-exp (map 2nd (3rd datum))) (map parse-exp (cdddr datum)))]
						[(list-improper? (cadr datum)) 
							(eopl:error 'parse-exp "declarations in ~s-expression not a list ~s" datum)]
						[(not (list-pair? (cadr datum))) 
							(eopl:error 'parse-exp "declaration in ~s-exp must be a list of length 2 ~s" datum)]
						[(not (list-all-symbols? (cadr datum))) 
							(eopl:error 'parse-exp "vars in ~s-exp must be symbols ~s" datum)]
						[else 
							(let-exp (pair-firsts (2nd datum)) (map parse-exp (pair-seconds (2nd datum))) (map parse-exp (cddr datum)))])]
				[(eqv? (car datum) 'lambda)
					(cond
						[(null? (cddr datum)) 
							(eopl:error 'parse-exp "Error in parse-exp: lambda-expression: incorrect length: ~s" datum)]
						[(symbol? (2nd datum)) 
							(improper-lambda-exp (list (2nd datum)) (map parse-exp (cddr datum)))]
						[(not (list? (2nd datum))) 
							(improper-lambda-exp (improper-list->list (2nd datum)) (map parse-exp (cddr datum)))]
						[(not (andmap symbol? (cadr datum))) 
							(eopl:error 'parse-exp "lambda's formal arguments ~s must all be symbols" (cadr datum))]
						[else 
							(lambda-exp (2nd datum) (map parse-exp (cddr datum)))])]
				[(eqv? (car datum) 'let*)
					(cond
						[(null? (cddr datum)) 
							(eopl:error 'parse-exp "~s-expression has no body ~s" datum)]
						[(not (list? (cadr datum))) 
							(eopl:error 'parse-exp "parameters are invalid ~s" datum)]
						[(list-improper? (cadr datum)) 
							(eopl:error 'parse-exp "declarations in ~s-expression not a list ~s" datum)]
						[(not (list-pair? (cadr datum))) 
							(eopl:error 'parse-exp "declaration in ~s-exp must be a list of length 2 ~s" datum)]
						[(not (list-all-symbols? (cadr datum))) 
							(eopl:error 'parse-exp "vars in ~s-exp must be symbols ~s" datum)]
						[else 
							(let*-exp (pair-firsts (2nd datum)) (map parse-exp (pair-seconds (2nd datum))) (map parse-exp (cddr datum)))])]
				[(eqv? (car datum) 'while)
					(cond
						[(or (null? (cdr datum)) (null? (cddr datum))) 
							(eopl:error 'parse-exp "while-exp ~s does not contain correct amount of expressions" datum)]
						[else 
							(while-exp (parse-exp (2nd datum)) (map parse-exp (cddr datum)))])]
				[(eqv? (car datum) 'case)
					(cond
						[(null? (cddr datum)) 
							(eopl:error 'parse-exp "case-exp does not contain correct number of arguments" datum)]
						[else 
							(case-exp (parse-exp (2nd datum)) (map 1st (cddr datum)) (map parse-exp (map 2nd (cddr datum))))])]
				[(eqv? (car datum) 'letrec)
					(cond
						[(null? datum) 
							(eopl:error 'parse-exp "letrec-exp does not contain correct number of arguments" datum)]
						[else 
							(letrec-exp 
								(map 1st (2nd datum)) (map 2nd (map 2nd (2nd datum))) 
								(map list (map parse-exp (map 3rd (map 2nd (2nd datum))))) (map parse-exp (cddr datum)))])]
				[(or (eqv? (car datum) 'define) (eqv? (car datum) 'set!))
					(cond
						[(not (= (length datum) 3)) 
							(eopl:error 'parse-exp "define-exp does not contain correct number of arguments" datum)]
						[else 
							(define-exp (2nd datum) (parse-exp (3rd datum)))])]
				[(eqv? (1st datum) 'and)
                    (and-exp (map parse-exp (cdr datum)))]
                [(eqv? (1st datum) 'or)
                    (or-exp (map parse-exp (cdr datum)))]
				[else (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum)))])]
			[else (eopl:error 'parse-exp "bad expression: ~s" datum)])))



;-------------------+
;                   |
;    ENVIRONMENTS   |
;                   |
;-------------------+


; Environment definitions for CSSE 304 Scheme interpreter.  
; Based on EoPL sections 2.2 and 2.3

(define empty-env
	(lambda ()
		(empty-env-record)))

(define extend-env
	(lambda (syms vals env)
		(extended-env-record syms (map cell vals) env)))

(define extend-env-recursively
	(lambda (proc-names ids bodies old-env)
		(recursively-extended-env-record proc-names ids bodies old-env)))

(define list-find-position
	(lambda (sym los)
		(list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
	(lambda (pred ls)
		(cond
			((null? ls) #f)
			((pred (car ls)) 0)
			(else (let ((list-index-r (list-index pred (cdr ls))))
				(if (number? list-index-r)
					(+ 1 list-index-r)
					#f))))))

(define deref
	(lambda (ref)
		(cell-ref ref)))

(define set-ref!
	(lambda (ref val)
		(cell-set! ref val)))


(define apply-env-ref				; look up sym in env
  (lambda (env sym succeed fail) 
    (cases environment env 
      [empty-env-record ()
      (fail)]
      [extended-env-record (syms vals env)
      (let ([pos (list-find-position sym syms)])
      	(if (number? pos)
      		(succeed (list-ref vals pos))
      		(apply-env-ref env sym succeed fail)))]
      [recursively-extended-env-record (proc-names idss bodiess old-env)
			(let ([pos (list-find-position sym proc-names)])
				(if (number? pos)
					(let ([id-list (list-ref idss pos)] [body-list (list-ref bodiess pos)])
						(if (list? id-list)
							(cell (closure id-list body-list env))
							(cell (improper-closure (improper-list->list id-list) body-list env))))
					(apply-env-ref old-env sym succeed fail)))])))
 

(define apply-env
	(lambda (env sym succeed fail)
		(deref (apply-env-ref env sym succeed fail))))


;-----------------------+
;                       |
;    SYNTAX EXPANSION   |
;                       |
;-----------------------+


(define map-syntax-expand
	(lambda (ls)
		(cond
			[(null? ls) '()]
			[else (cons (map syntax-expand (car ls)) (map-syntax-expand (cdr ls)))])))

(define syntax-expand
	(lambda (exp)
		(cases expression exp
			[lit-exp (id)
				(lit-exp id)]
			[case-lambda-exp (params bodies)
				(case-lambda-exp params (map-syntax-expand bodies))]
			[let-exp (vars exps bodies)
				(app-exp (lambda-exp vars (map syntax-expand bodies)) (map syntax-expand exps))]
			[let*-exp (vars exps bodies)
				(syntax-expand (let*->let vars exps bodies))]
			[case-exp (test-exp compare-exp outcome-exp)
				(syntax-expand (case->if (syntax-expand test-exp) compare-exp (map syntax-expand outcome-exp)))]
			[letrec-exp (proc-names ids bodies letrec-bodies)
				(letrec-exp proc-names ids (map-syntax-expand bodies) (map syntax-expand letrec-bodies))]
			[named-let-exp (name idss bodiess letrec-bodies)
                (letrec-exp (list name) (list idss) (list letrec-bodies) (list (app-exp (var-exp name) bodiess)))]
            [and-exp (body)
            	(and-exp (map syntax-expand body))]
            [or-exp (body)
            	(or-exp (map syntax-expand body))]
			[app-exp (rator rands)
				(cases expression rator
					[var-exp (id)
						(cond
							[(eq? id 'or) (or->if rands)]
							[(eq? id 'and) (and->if rands)]
							[(eq? id 'cond) (cond->if rands)]
							[(eq? id 'begin) (app-exp (lambda-exp '() (map syntax-expand rands)) '())]
							[(eq? id 'map) (app-exp (var-exp id) (map syntax-expand rands))]
							[else exp])]
					[else (app-exp (syntax-expand rator) (map syntax-expand rands))])]
			[lambda-exp (vars bodies)
				(lambda-exp vars (map syntax-expand bodies))]
			[improper-lambda-exp (vars bodies)
				(improper-lambda-exp vars (map syntax-expand bodies))]
			[if-exp (test-exp then-exp else-exp)
				(if-exp (syntax-expand test-exp) (syntax-expand then-exp) (syntax-expand else-exp))]
			[if-no-else-exp (test-exp then-exp)
				(if-no-else-exp (syntax-expand test-exp) (syntax-expand then-exp))]	
			[define-exp (var body-exp)
				(define-exp var (syntax-expand body-exp))]
			[else exp])))

(define or->if
	(lambda (rands)
		(if (null? rands)
			(lit-exp #f)
			(let ([true-val (syntax-expand (car rands))])
				(cond
					[(null? (cdr rands)) (if-exp true-val true-val (lit-exp #f))]
					[else (if-exp true-val true-val (or->if (cdr rands)))])))))

(define and->if
	(lambda (rands)
		(cond
			[(null? rands) (lit-exp #t)]
			[(null? (cdr rands)) (if-exp (syntax-expand (car rands)) (syntax-expand (car rands)) (lit-exp #f))]
			[else (if-exp (syntax-expand (car rands)) (and->if (cdr rands)) (lit-exp #f))])))
	; (if (null? body)
    ;                 (lit-exp #t)
    ;                 (if-else-exp 
    ;                     (syntax-expand (car body)) 
    ;                     (cons 'and-exp (list (cdr body))) 
    ;                     (lit-exp #f)))

(define cond->if
	(lambda (args)
		(cases expression (1st args)
			[app-exp (rator rands)
				(cases expression rator
					[var-exp (id)
						(if (eq? id 'else)
							(if-no-else-exp (lit-exp #t) (syntax-expand (car rands)))
							(if-exp (syntax-expand (var-exp id)) (syntax-expand (car rands)) (cond->if (cdr args))))]
					[else (if (null? (cdr args))
						(if-no-else-exp (syntax-expand rator) (syntax-expand (car rands)))
						(if-exp (syntax-expand rator) (syntax-expand (car rands)) (cond->if (cdr args))))])]
			[else args])))

(define let*->let
	(lambda (vars exps bodies)
		(cond
			[(null? (cdr vars)) (let-exp (list (car vars)) (list (car exps)) bodies)]
			[else (let-exp (list (car vars)) (list (car exps)) (list (let*->let (cdr vars) (cdr exps) bodies)))])))


(define case->if
	(lambda (test compare outcome)
		(cond
			[(and (null? (cddr compare)) (equal? (cadr compare) 'else)) (if-exp (app-exp (var-exp 'list-contains) (test (car compare))) (car outcome) (cadr outcome))]
			[(and (null? (cdr compare)) (not (equal? (car compare) 'else))) (if-no-else-exp (app-exp (var-exp 'list-contains) (test (car compare))) (car outcome))]
			[else (if-exp (app-exp (var-exp 'list-contains) (test (car compare))) (car outcome) (case->if test (cdr compare) (cdr outcome)))])))


;-------------------+
;                   |
;    INTERPRETER    |
;                   |
;-------------------+


; top-level-eval evaluates a form in the global environment

(define top-level-eval
	(lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form global-env)))

; eval-exp is the main component of the interpreter

(define eval-exp
	(lambda (exp env)
		(cases expression exp
			[lit-exp (datum) (if (list? datum) (car datum) datum)]
			[var-exp (id)
				(apply-env env id 	; look up its value in local environment
					(lambda (x) x) 	; success 
					(lambda () 		; procedure to look in the global environment (fail in local)
						(apply-env-ref global-env id
							(lambda (x) x) ; found in the global environment
							(lambda () (eopl:error 'apply-env "variable not found in environment: ~s" id)))))]
			[app-exp (rator rands)
				(let ([proc-value (eval-exp rator env)] [args (eval-rands rands env)])
				(apply-proc proc-value args))]
			[case-lambda-exp (params bodies)
				(case-closure params bodies env)]
			[let-exp (vars exps bodies)
				(eval-bodies
					bodies
					(extend-env vars (eval-rands exps env) env))]
			[lambda-exp (vars bodies)
				(closure vars bodies env)]
			[if-exp (test-exp then-exp else-exp)
				(if (eval-exp test-exp env)
					(eval-exp then-exp env)
					(eval-exp else-exp env))]
			[if-no-else-exp (test-exp then-exp)
				(if (eval-exp test-exp env)
					(eval-exp then-exp env))]
			[improper-lambda-exp (vars bodies)
				(improper-closure vars bodies env)]
			[while-exp (test-exp bodies)
				(while-eval test-exp bodies env)]
			[and-exp (body)
        		(eval-exp 
          			body 
        			env)]
			[or-exp (body)
        		(if (null? body)
           			#f
           			(let ([next (eval-exp (car body) env)])
             			(if next next (eval-exp [or-exp (cdr body)] env))))]
			[letrec-exp (names idss bodiess letrec-bodies)
        		(eval-bodies letrec-bodies
         			(extend-env-recursively
          				names idss (map list (map syntax-expand (map car bodiess))) env))]
			[define-exp (var body-exp)
				(set-ref! (apply-env-ref env var
								(lambda (x) x) 				; procedure to call if it is in the environment
								(lambda () 					; procedure to look in the global environment
									(apply-env-ref global-env var
										(lambda (x) x) 		; found in the global environment
										(lambda () (begin (set! global-env (extend-env (list var) (list 'to-change) global-env))
													(apply-env-ref global-env var
														(lambda (x) x)
														(lambda () (eopl:error 'apply-env "variable not found in environment: ~s" id)))))
									))) (eval-exp body-exp env))]
			[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

; evaluate the list of operands, putting results into a list

(define while-eval
	(lambda (test-exp bodies env)
		(if (eval-exp test-exp env)
			(begin 
				(eval-bodies bodies env)
				(while-eval test-exp bodies env)))))

(define eval-bodies
	(lambda (bodies env)
		(if (null? (cdr bodies))
			(eval-exp (car bodies) env)
			(begin
				(eval-exp (car bodies) env)
				(eval-bodies (cdr bodies) env)))))

(define eval-rands
	(lambda (rands env)
		(map (lambda (e)
			(eval-exp e env)) rands)))

(define improper-args->args
	(lambda (vars args)
		(cond
			[(null? (cdr vars)) (list args)]  ; reached last element of vars, rest of args needs to be packaged into list
			[else (cons (car args) (improper-args->args (cdr vars) (cdr args)))])))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define improper-length
	(lambda (ls len)
		(cond
			[(symbol? ls) 0] 
			[(not (list? (cdr ls))) (+ len 1)]
			[else (improper-length (cdr ls) (+ 1 len))])))

(define pick-lambda
	(lambda (params bodies env args-length args)
		(let find-lambda 
			([list-of-params params] 
				[index 0] [improper '()])
			(if (null? list-of-params)
				(let find-improper-lambda 
					([len (improper-length (list-ref params (car improper)) 1)] 
						[improper-ls improper])
					(if (>= args-length len)
						(improper-closure 
							(improper-args->args (list-ref params (car improper-ls)) args) 
							(list-ref bodies (car improper-ls)) 
							env)
						(find-improper-lambda 
							(improper-length (list-ref params (cadr improper-ls))) 
							(cdr improper-ls))))
				(if (not (list? (car list-of-params))) 							; check if params is an improper set of arguments
					(if (null? improper)										; improper still null so either make new list or add to old
						(find-lambda (cdr list-of-params) (+ index 1) (list index))
						(find-lambda (cdr list-of-params) (+ index 1) (append improper (list index))))
					(if (= args-length (length (car list-of-params)))			; check if this is right set of params
						(closure (list-ref params index) (list-ref bodies index) env)			; create closure if right one
						(find-lambda (cdr list-of-params) (+ index 1) improper)))))))			; otherwise keep going

(define apply-proc
	(lambda (proc-value args)
		(cases proc-val proc-value
			[prim-proc (op) (apply-prim-proc op args)]
			[closure (vars bodies env) 
				(eval-bodies bodies (extend-env vars args env))]
			[improper-closure (vars bodies env)
				(eval-bodies bodies (extend-env vars (improper-args->args vars args) env))]
			[case-closure (params bodies env)
				(apply-proc (pick-lambda params bodies env (length args) args) args)]
			[else (eopl:error 'apply-proc
				"Attempt to apply bad procedure: ~s" 
				proc-value)])))

(define *prim-proc-names* '(+ - * / add1 sub1 quotient cons = zero? not < > <= >= car cdr list null? eq? equal? atom? length list->vector list? 
							pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set!
							display newline caar cadr cdar cddr caaar caadr cadar cdaar caddr cdadr cddar cdddr map apply map2 ormap list-contains 
							list-tail eqv? append assq))

(define make-init-env         ; for now, our initial global environment only contains 
	(lambda ()
		(extend-env            ; procedure names.  Recall that an environment associates
     		*prim-proc-names*   ;  a value (not an expression) with an identifier.
     		(map prim-proc *prim-proc-names*)
     		(empty-env))))

(define global-env (make-init-env))

(define reset-global-env
	(lambda () (set! global-env (make-init-env))))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
	(lambda (prim-proc args)
		(case prim-proc
			[(+) (apply + args)]
			[(-) (apply - args)]
			[(*) (apply * args)]
			[(/) (apply / args)]
			[(add1) (+ (1st args) 1)]
			[(sub1) (- (1st args) 1)]
			[(quotient) (quotient (1st args) (2nd args))]
			[(cons) (cons (1st args) (2nd args))]
			[(append) (append (1st args) (2nd args))]
			[(=) (= (1st args) (2nd args))]
			[(zero?) (zero? (1st args))]
			[(not) (not (1st args))]
			[(<) (< (1st args) (2nd args))]
			[(>) (> (1st args) (2nd args))]
			[(<=) (>= (1st args) (2nd args))]
			[(>=) (>= (1st args) (2nd args))]
			[(list) args]
			[(null?) (null? (1st args))]
			[(eq?) (eq? (1st args) (2nd args))]
			[(equal?) (equal? (1st args) (2nd args))]
			[(eqv?) (eqv? (1st args) (2nd args))]
			[(atom?) (atom? (1st args))]
			[(number?) (number? (1st args))]
			[(symbol?) (symbol? (1st args))]
			[(length) (length (1st args))]
			[(list?) (list? (1st args))]
			[(pair?) (pair? (1st args))]
			[(procedure?) (proc-val? (1st args))]
			[(list-tail) (list-tail (1st args) (2nd args))]
			[(list->vector) (list->vector (1st args))]
			[(vector->list) (vector->list (1st args))]
			[(vector) (apply vector args)]
			[(make-vector) (make-vector (1st args) (2nd args))] 
			[(vector-ref) (vector-ref (1st args) (2nd args))]
			[(vector?) (vector? (1st args))]
			[(set-car!) (set-car! (1st args) (2nd args))]
			[(set-cdr!) (set-cdr! (1st args) (2nd args))]
			[(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
			[(display) (display (1st args))]
			[(newline) (newline)]
			[(car) (car (1st args))]
			[(cdr) (cdr (1st args))]
			[(caar) (caar (1st args))]
			[(cadr) (cadr (1st args))]
			[(cdar) (cdar (1st args))]
			[(cddr) (cddr (1st args))]
			[(caaar) (caaar (1st args))]
			[(caadr) (caadr (1st args))]
			[(cadar) (cadar (1st args))]
			[(cdaar) (cdaar (1st args))]
			[(caddr) (caddr (1st args))]
			[(cdadr) (cdadr (1st args))]
			[(cddar) (cddar (1st args))]
			[(cdddr) (cdddr (1st args))]
			[(map) (map-prim-proc (1st args) (2nd args))]
			[(apply) (apply-proc (1st args) (2nd args))]
			[(map2) (map2-prim-proc (1st args) (2nd args) (3rd args))]
			[(ormap) (check-true (map2-prim-proc (1st args) (2nd args) (3rd args)))]
			[(list-contains) (check-contains (1st args) (2nd args))]
			[(assq) (assq (1st args) (2nd args))]
			[else (error 'apply-prim-proc 
				"Bad primitive procedure name: ~s" 
				prim-proc)])))

(define map-prim-proc
	(lambda (proc args)
		(cond
			[(null? args) '()]
			[else (cons (apply-proc proc (list (car args))) (map-prim-proc proc (cdr args)))])))

(define map2-prim-proc
	(lambda (proc arg1 arg2)
		(cond
			[(null? arg1) '()]
			[else (cons (apply-proc proc (list (car arg1) (car arg2))) (map2-prim-proc proc (cdr arg1) (cdr arg2)))])))

(define check-true
	(lambda (ls)
		(cond
			[(null? ls) #f]
			[(car ls) (car ls)]
			[else (check-true (cdr ls))])))

(define check-contains
	(lambda (val ls)
		(cond
			[(null? ls) #f]
			[(eqv? val (car ls)) #t]
			[else (check-contains val (cdr ls))])))

(define rep      ; "read-eval-print" loop.
	(lambda ()
		(display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
	(lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))