(library (Compiler assign-registers)
	(export assign-registers)
	(import 
	  ;; Load Chez Scheme primitives:
	  (chezscheme)
	  ;; Load provided compiler framework:
	  (Framework driver)
	  (Framework wrappers)
	  (Framework match)
	  (Framework helpers)
	  (Framework prims)
	  (Compiler helper))

	(define-who assign-registers
		(define (graph-remove x graph)
			(let ([graph (map (lambda (y) (cons (car y) (remove x (cdr y)))) graph)])
				(remove (assq x graph) graph)))
		(define (low-degree cur uvar* conflict*)
			(cond
				[(null? uvar*) cur]
				[(< (length registers) (length (assq cur conflict*)))
				(low-degree (car uvar*) (cdr uvar*) conflict*)]
				[else (low-degree cur (cdr uvar*) conflict*)]
				))
		(define (select* uvar* conflict* spills)
			(if (null? uvar*) (values '() spills)
				(let*-values
            ([(uvar) (low-degree (car uvar*) (cdr uvar*) conflict*)]
            	[(alist spills) (select* (remove uvar uvar*) (graph-remove uvar conflict*) spills)]
            	[(conflicts)
            	(map (lambda (x)
            		(if (assq x alist) (cadr (assq x alist)) x))
            	(assq uvar conflict*))]
            	[(avail) (difference registers conflicts)])
            (if (null? avail) (values alist (set-cons uvar spills))
            	(values `((,uvar ,(car avail)) . ,alist) spills)))))
		(define Body
			(lambda (body)
				(match body
					[(locals (,locals ...)
						(ulocals (,ulocals ...)
							(locate ,locate
								(frame-conflict ,fgraph
									(register-conflict ,rgraph ,tail)))))
					(let-values ([(bind* spills) (select* (union ulocals locals) rgraph '())])
						(if (null? spills) `(locate (,locate ... ,bind* ...) ,tail)
							`(locals ,(difference locals spills)
								(ulocals ,(difference ulocals spills)
									(spills ,spills
										(locate ,locate
											(frame-conflict ,fgraph ,tail)))))))]
					[,x (error who "invalid Body ~s" x)])))
		(lambda (x)
			(match x
				[(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
				`(letrec ([,label* (lambda () ,body*)] ...) ,body)]
				[,x (error who "invalid Program ~s" x)])))
)