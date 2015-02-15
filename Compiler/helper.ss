(library (Compiler helper)
	(export 
   		do-uncover-conflict
    	relop?
     	binop?
      	graph-remove
        finalize
    )
	(import
		(chezscheme)
		(Framework match)
		(Framework helpers)
  		(Framework prims))

	(define warn-if-dead-at-assignment (make-parameter #f))

	(define do-uncover-conflict
		(lambda (tail uvar* who fixed?)
			(define add-conflicts!
				(lambda (ct lhs live*)
					(define add-conflict!
						(lambda (var1 var2)
							(let ([a (assq var1 ct)])
								(set-cdr! a (set-cons var2 (cdr a))))))
					(when (uvar? lhs)
						(for-each
							(lambda (live) (add-conflict! lhs live))
							live*))
					(for-each
						(lambda (live) (when (uvar? live) (add-conflict! live lhs)))
						live*)))
			(define Triv (lambda (x) (if (or (uvar? x) (fixed? x)) `(,x) '())))
			(define Effect*
				(lambda (x live* ct)
					(match x
						[() live*]
						[(,ef* ... ,ef) (Effect* ef* (Effect ef live* ct) ct)]
						[,x (error who "invalid Effect* list ~s" x)])))
			(define Effect
				(lambda (x live* ct)
					(match x
						[(nop) live*]
						[(if ,test ,[c-live*] ,[a-live*]) (Pred test c-live* a-live* ct)]
						[(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
						[(set! ,lhs ,rhs)
						(guard (or (uvar? lhs) (fixed? lhs)) (not (memq lhs live*)))
						(when (warn-if-dead-at-assignment)
							(warning who "~s is not live at assignment ~s" lhs
								`(set! ,lhs ,rhs)))
						(Effect `(set! ,lhs ,rhs) (cons lhs live*) ct)]
						[(set! ,lhs (,binop ,[Triv -> x-live*] ,[Triv -> y-live*]))
						(let ([live* (difference live* `(,lhs))])
							(when (or (uvar? lhs) (fixed? lhs))
								(add-conflicts! ct lhs live*))
							(union x-live* y-live* live*))]
						[(set! ,lhs ,var)
						(let ([live* (difference live* `(,lhs))])
							(when (or (uvar? lhs) (fixed? lhs))
								(add-conflicts! ct lhs (remq var live*)))
							(union (Triv var) live*))]
						[,x (error who "invalid Effect list ~s" x)])))
			(define Pred
				(lambda (x t-live* f-live* ct)
					(match x
						[(true) t-live*]
						[(false) f-live*]
						[(if ,test ,[c-live*] ,[a-live*]) (Pred test c-live* a-live* ct)]
						[(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
						[(,relop ,[Triv -> x-live*] ,[Triv -> y-live*])
						(union t-live* f-live* x-live* y-live*)]
						[,x (error who "invalid Pred ~s" x)])))
			(define Tail
				(lambda (x ct)
					(match x
						[(begin ,ef* ... ,[live*]) (Effect* ef* live* ct)]
						[(if ,test ,[c-live*] ,[a-live*]) (Pred test c-live* a-live* ct)]
						[(,[Triv -> target-live*] ,live* ...)
						(union target-live*
							(filter
								(lambda (x) (or (fixed? x) (uvar? x)))
								live*))]
						[,x (error who "invalid Tail ~s" x)])))
			(let ([ct (map (lambda (x) (cons x '())) uvar*)])
				(let ([uvar* (filter uvar? (Tail tail ct))])
					(unless (null? uvar*)
						(warning who "found variables ~s live on entry" uvar*)))
				ct)))
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
					[(,op ,[x] ,[y]) (guard (or (binop? op) (relop? op)))
					`(,op ,x ,y)]
					[(,[triv] ,[live*] ...)
					(if final? `(,triv) `(,triv ,live* ...))]
					[,v (guard (uvar? v)) (lookup v env)]
					[,x x]))) 
);end library
