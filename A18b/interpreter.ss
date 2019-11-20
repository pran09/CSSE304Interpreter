; Vanshika Reddy and Praneet Chakraborty


(define *prim-proc-names* '(call/cc + - * / add1 sub1 zero? not = < > >= <= cons car cdr caar cdar cadr cddr
								caddr caadr cdadr cdddr caaar cadar cdaar cddar list null? append list-tail
								assq eq? eqv? equal? atom? length list->vector list? pair? procedure?
								vector->list vector make-vector vector-ref vector? number?
								symbol? set-car! set-cdr! vector-set! display newline
								map apply even? quotient void exit-list call-with-values values call/cc))

(define make-init-env
	(lambda ()
		(extend-env
		*prim-proc-names*
		(map prim-proc *prim-proc-names*)
		(empty-env))))

(define global-env (make-init-env))

(define reset-global-env
	(lambda () 
		(set! global-env (make-init-env))))

(define identity-proc
	(lambda (x) x))

; top-level-eval evaluates a form in the global environment
(define top-level-eval
	(lambda (form)
		; later we may add things that are not expressions.
		(eval-exp form (empty-env) (init-k))))

; eval-exp is the main component of the interpreter

(define eval-exp
	(lambda (exp env k)
		(cases expression exp
			[lit-exp (datum) (apply-k k datum)]
			[var-exp (id)
				(apply-env env id ; look up its value.
					(lambda (x)
						(apply-k k x)) ; procedure to call if id is in the environment 
					(lambda () 
					   	(apply-env global-env id
					   		(lambda (x)
								(apply-k k x))
					   		(lambda ()
					   			(eopl:error 'apply-env ; procedure to call if id not in env
					   				"variable not found in environment: ~s" id)))))]
			[lambda-exp (ids body)
				(apply-k k (closure ids body env))]
			[lambdavar-exp (datum body)
				(apply-k k (closure-var datum body env))]
			[lambda-improper-exp (datum idlist body)
				(apply-k k (closure-improper datum idlist body env))]
			[if-exp (bool thencase)
				(eval-exp bool env (if-k thencase env k))]
			[if-else-exp (bool thencase elsecase)
				(eval-exp bool env (if-else-k thencase elsecase env k))]
			[set!-exp (id exp)
				(eval-exp exp env
					(set-k
						(apply-env-ref env id
							identity-proc ; procedure to call if id is in the environment 
							(lambda () 
							   	(apply-env-ref global-env id
							   		identity-proc
							   		(lambda ()
							   			(eopl:error 'apply-env-ref ; procedure to call if id not in env
							   				"variable not found in environment: ~s" id)))))
						k))]
			[define-exp (id exp)
				(eval-exp exp env (define-k id k))]
			[app-exp (rator rands)
				(eval-exp rator env (rator-k rands env k))]
			[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

(define case-or-expression
	(lambda (key test)
		(syntax-expand (or-exp (map (lambda (x) (app-exp (var-exp `equal?) (list key x))) test)))))

(define build-temps
	(lambda (values)
		(map syntax->datum (generate-temporaries values))))

(define make-set!-list
	(lambda (ids values)
		(if (null? ids)
			'()
			(cons
				(set!-exp (car ids) (parse-exp (car values)))
				(make-set!-list (cdr ids) (cdr values))))))



; evaluate the list of operands, putting results into a list

(define eval-rands
	(lambda (rands env k)
		(map-cps (lambda (x k) (eval-exp x env k)) rands k)))


(define extract-extra-args-closure-improper
	(lambda (params args)
		(if (null? params)
			(list args)
			(cons (car args) (extract-extra-args-closure-improper (cdr params) (cdr args))))))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
	(lambda (proc-value args k)
		(cases proc-val proc-value
			[continuation-proc (k)
				(apply-k k (car args))]
			[prim-proc (op)
				(apply-prim-proc op args k)]
			[closure (params bodies env)
				(apply-k
					(eval-bodies-k
						bodies
						(extend-env params args env)
						k)
					k)]
			[closure-var (ids bodies env)
				(apply-k
					(eval-bodies-k
						bodies
						(extend-env (list ids) (list args) env)
						k)
					k)]
			[closure-improper (params listsymbol bodies env)
				(apply-k
					(eval-bodies-k
						bodies
						(extend-env 
							(append params (list listsymbol)) 
							(extract-extra-args-closure-improper params args)
							env)
						k)
					k)]
			[else (error 'apply-proc
				"Attempt to apply bad procedure: ~s" 
				proc-value)])))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
	(lambda (prim-proc args k)
		(case prim-proc
			[(+) (apply-k k (apply + args))]
			[(-) (apply-k k (apply - args))]
			[(*) (apply-k k (apply * args))]
			[(/) (apply-k k (apply / args))]
			[(add1) (apply-k k (+ (1st args) 1))]
			[(sub1) (apply-k k (- (1st args) 1))]
			[(zero?) (apply-k k (= (1st args) 0))]
			[(not) (apply-k k (not (1st args)))]
			[(=) (apply-k k (= (1st args) (2nd args)))]
			[(<) (apply-k k (< (1st args) (2nd args)))]
			[(>) (apply-k k (> (1st args) (2nd args)))]
			[(>=) (apply-k k (>= (1st args) (2nd args)))]
			[(<=) (apply-k k (<= (1st args) (2nd args)))]
			[(cons) (apply-k k (cons (1st args) (2nd args)))]
			[(car) (apply-k k (car (1st args)))]
			[(cdr) (apply-k k (cdr (1st args)))]
			[(caar) (apply-k k (caar (1st args)))]
			[(cdar) (apply-k k (cdar (1st args)))]
			[(cadr) (apply-k k (cadr (1st args)))]
			[(cddr) (apply-k k (cddr (1st args)))]
			[(caddr) (apply-k k (caddr (1st args)))]
			[(caadr) (apply-k k (caadr (1st args)))]
			[(cdadr) (apply-k k (cdadr (1st args)))]
			[(cdddr) (apply-k k (cdddr (1st args)))]
			[(caaar) (apply-k k (caaar (1st args)))]
			[(cadar) (apply-k k (cadar (1st args)))]
			[(cdaar) (apply-k k (cdaar (1st args)))]
			[(cddar) (apply-k k (cddar (1st args)))]
			[(list) (apply-k k args)]
			[(null?) (apply-k k (null? (1st args)))]
			[(append) (apply-k k (append (1st args) (2nd args)))]
			[(list-tail) (apply-k k (list-tail (1st args) (2nd args)))]
			[(assq) (apply-k k (assq (1st args) (2nd args)))]
			[(eq?) (apply-k k (eq? (1st args) (2nd args)))]
			[(eqv?) (apply-k k (eqv? (1st args) (2nd args)))]
			[(equal?) (apply-k k (equal? (1st args) (2nd args)))]
			[(atom?) (apply-k k (not (pair? (1st args))))]
			[(length) (apply-k k (length (1st args)))]
			[(list->vector) (apply-k k (list->vector (1st args)))]
			[(list?) (apply-k k (list? (1st args)))]
			[(pair?) (apply-k k (pair? (1st args)))]
			[(procedure?) (apply-k k (proc-val? (1st args)))]
			[(vector->list) (apply-k k (vector->list (1st args)))]
			[(vector) (apply-k k (list->vector args))]
			[(make-vector) (apply-k k (make-vector (1st args)))]
			[(vector-ref) (apply-k k (vector-ref (1st args) (2nd args)))]
			[(vector?) (apply-k k (vector? (1st args)))]
			[(number?) (apply-k k (number? (1st args)))]
			[(symbol?) (apply-k k (symbol? (1st args)))]
			[(set-car!) (apply-k k (set-car! (1st args) (2nd args)))]
			[(set-cdr!) (apply-k k (set-cdr! (1st args) (2nd args)))]
			[(vector-set!) (apply-k k (vector-set! (1st args) (2nd args) (3rd args)))]
			[(display) (apply-k k (display (1st args)))]
			[(newline) (apply-k k (newline))]
			[(map)
				(map-cps
					(lambda (x k)
						(apply-proc (car args) (list x) k))
					(cadr args)
					k)]
			[(apply) (apply-proc (1st args) (2nd args) k)]
			[(even?) (apply-k k (even? (1st args)))]
			[(quotient) (apply-k k (quotient (1st args) (2nd args)))]
			[(void) (apply-k k (void))]
			[(exit-list) (apply-k (init-k) args)]
			[(call/cc) (apply-proc (car args) (list (continuation-proc k)) k)]
			[else (error 'apply-prim-proc 
				"Bad primitive procedure name: ~s" 
				prim-op)])))

(define rep      ; "read-eval-print" loop.
	(lambda ()
		(display "--> ")
		;; notice that we don't save changes to the environment...
		(let ([answer (eval-one-exp (read))])
			;; TODO: are there answers that should display differently?
			(eopl:pretty-print answer) (newline)
			(rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
	(lambda (x) (top-level-eval (syntax-expand (parse-exp x)))))
