(library (Compiler expose-allocation-pointer)
	(export expose-allocation-pointer)
	(import
   	;; Load Chez Scheme primitives:
   	(chezscheme)
   	;; Load compiler framework:
   	(Framework match)
   	(Framework helpers)
   	(Compiler helper)
   	)

	(define-who expose-allocation-pointer
		(lambda(x)
			(match x
				[(letrec ([,label* (lambda () ,[body*])] ...) ,[body])
				`(letrec ([,label* (lambda () ,body*)] ...) ,body)]
				[(locals (,local* ...)
					(ulocals (,ulocal* ...)
						(locate ([,uvar* ,loc*] ...)
							(frame-conflict ,fgraph
								,[tail]))))
				`(locals (,local* ...)
					(ulocals (,ulocal* ...)
						(locate ([,uvar* ,loc*] ...)
							(frame-conflict ,fgraph ,tail))))]
				[(locate (,home* ...) ,[tail])
				`(locate (,home* ...) ,tail)]
				[(begin ,[effect*] ... ,[tail])
				`(begin ,effect* ... ,tail)]
				[(if ,[pred] ,[conseq] ,[altern])
				`(if ,pred ,conseq ,altern)]
				[(set! ,uvar (alloc ,[tr]))
				(let ([apr allocation-pointer-register])
					(make-begin `((set! ,uvar ,apr)
						(set! ,apr (+ ,apr ,tr)))))]
    			[(,[triv] ,[loc*] ...) `(,triv ,loc* ...)]
				[(,[a]) a]
				[,x x])))
);