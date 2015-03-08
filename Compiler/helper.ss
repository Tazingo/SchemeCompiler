(library (Compiler helper)
	(export 
		uncover-conflicts
		relop?
		binop?
		graph-remove
		finalize
		triv?
		var?
		loc?
		)
	(import
		(chezscheme)
		(Framework match)
		(Framework helpers)
		(Framework prims))


	(define relop?
		(lambda (x)
			(memq x '(= < <= >= >))))
	(define binop? 
		(lambda(op)
			(isBinop op)))
	(define (graph-remove x graph)
		(let ([graph (map (lambda (y) (cons (car y) (remove x (cdr y)))) graph)])
			(remove (assq x graph) graph)))



	(define finalize
		(lambda (x env final?)
			(define lookup
				(lambda (v env)
					(let ((slot (assq v env)))
						(if slot (cdr slot) v))))
			(match x
				[(letrec ([,label* (lambda () , [bd*])] ...) , [bd])
				`(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
				[(locals (,local* ...)
					(ulocals (,ulocal* ...)
						(locate ([,uvar* ,loc*] ...)
							(frame-conflict ,ct ,tail))))
				`(locals (,local* ...)
					(ulocals (,ulocal* ...)
						(locate ([,uvar* ,loc*] ...)
							(frame-conflict ,ct
								,(finalize tail `((,uvar* . ,loc*) ...) final?)))))]
				[(locate ([,uvar* ,loc*] ...) ,tail)
				(if final?
					(finalize tail `((,uvar* . ,loc*) ...) final?)
					`(locate ([,uvar* ,loc*] ...) ,tail))]
				[(begin , [ef*] ... , [tail])
				`(begin ,ef* ... ,tail)]
				[(if , [test] , [conseq] , [altern])
				`(if ,test ,conseq ,altern)]
				[(set! ,[x] (,binop ,[y] ,[z]))
				`(set! ,x (,binop ,y ,z))]
				[(set! ,[x] ,[y])
				(if (eq? x y) `(nop) `(set! ,x ,y))]
				[(return-point ,label ,[tail])
				`(return-point ,label ,tail)]
				[(,op ,[x] ,[y]) (guard (or (binop? op) (relop? op)))
				`(,op ,x ,y)]
				[(,[triv] ,[live*] ...)
				(if final? `(,triv) `(,triv ,live* ...))]
				[,v (guard (uvar? v)) (lookup v env)]
				[,x x]))) 
(define (triv? exp)
	(or
		(var? exp)
		(int64? exp)
		(label? exp)
		)
	)
(define (var? exp)
	(or
		(uvar? exp)
		(loc? exp)
		)
	)
(define (loc? exp)
	(or (register? exp) (frame-var? exp))
	)

(define (uncover-conflicts tail uvar* who qual)
	(define call-live '())
	(define (graph-add! s conflict graph)
		(when (uvar? s)
			(let ([assoc (assq s graph)])
				(if assoc
					(set-cdr! assoc (set-cons conflict (cdr assoc)))
					(cons (list s conflict) graph)
					)))
		graph)
	(define (update-graph s conflicts graph)
		(if (or (qual s) (uvar? s))
			(let loop ([conflicts conflicts]
				[graph graph])
			(cond
				[(null? conflicts) graph]
				[else
				(loop (cdr conflicts)
					(graph-add! (car conflicts) s
						(graph-add! s (car conflicts) graph)))]
				))
			graph))

	(define (handle var ls)
		(if (or (uvar? var) (qual var)) (set-cons var ls) ls))

	(define (graph-union! g0 g1)
		(for-each (lambda (assoc)
			(let ([s (car assoc)]
				[flicts (cdr assoc)])
			(update-graph s flicts g1))) g0)
		g1 #|Pacman blocker|#
		)

	(define (Effect effect* effect graph live)
		(match effect
			[(begin ,e* ...) (Effect* (append effect* e*) graph live)]
			[(if ,pred ,conseq ,altern)
			(let*-values ([(ga lsa) (Effect '() altern graph live)]
				[(gc lsc) (Effect '() conseq graph live)]
				[(gp lsp) (Pred pred gc ga lsc lsa)])
			(Effect* effect* gp lsp))]
			[(nop) (Effect* effect* graph live)]
			[(return-point ,label ,tail)
			(let-values ([(gt lst) (Tail tail graph '())])
				(set! call-live (union call-live live))
				(Effect* effect* gt (union lst (filter (lambda (x) (or (uvar? x) (frame-var? x))) live))))]
			[(set! ,lhs (,binop ,rhs0 ,rhs1)) (guard (binop? binop))
			(let ([ls (remove lhs live)])
				(Effect* effect* (update-graph lhs ls graph) (handle rhs0 (handle rhs1 ls))))]
			[(set! ,lhs ,rhs)
			(let ([ls (remove lhs live)])
				(Effect* effect* (update-graph lhs ls graph) (handle rhs ls)))]))
	(define (Effect* effect* graph live)
		(match effect*
			[() (values graph live)]
			[(,effect* ... ,effect) (Effect effect* effect graph live)]))
	(define (Pred pred Cgraph Agraph Clive Alive)
		(match pred
			[(true) (values Cgraph Clive)]
			[(false) (values Agraph Alive)]
			[(begin ,effect* ... ,pred)
			(let*-values ([(gp lsp) (Pred pred Cgraph Agraph Clive Alive)]
				[(ge* lse*) (Effect* effect* gp lsp)])
			(values ge* lse*))]
			[(if ,pred ,conseq ,altern)
			(let*-values ([(ga lsa) (Pred altern Cgraph Agraph Clive Alive)]
				[(gc lsc) (Pred conseq Cgraph Agraph Clive Alive)]
				[(gp lsp) (Pred pred gc ga lsc lsa)])
			(values gp lsp)
			)]
			[(,relop ,triv0 ,triv1)
			(values (graph-union! Cgraph Agraph) (handle triv0 (handle triv1 (union Clive Alive))))]))
	(define (Tail tail graph live)
		(match tail
			[(begin ,effect* ... ,tail)
			(let*-values ([(gt lst) (Tail tail graph live)]
				[(ge* lse*) (Effect* effect* gt lst)])
			(values graph lse*))]
			[(if ,pred ,conseq ,altern)
			(let*-values ([(ga lsa) (Tail altern graph live)]
				[(gc lsc) (Tail conseq graph live)]
				[(gp lsp) (Pred pred gc ga lsc lsa)])
			(values graph lsp))
			]
			[(,triv ,loc* ...)
			(values graph (handle triv
				(union (filter (lambda (x) (or (uvar? x) (qual x))) loc*) live)))]))
	(let*-values ([(empty-graph) (map (lambda (s) (cons s '())) uvar*)]
		[(live-set) '()]
		[(graph lives) (Tail tail empty-graph live-set)])
	(values graph call-live))
	)





);end library
