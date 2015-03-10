(library (Compiler remove-complex-opera*)
	(export remove-complex-opera*)
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

	(define-who remove-complex-opera*
		(define (Body bd)
			(define new-local* '())
			(define (simple? xpr)
				(or (and (integer? xpr) (exact? xpr))
					(label? xpr)
					(uvar? xpr)
					(binop? xpr)
					(relop? xpr)
					(eq? 'alloc xpr)
					(eq? 'mref xpr)
					(eq? 'mset! xpr)
					))
			(define (new-t)
				(let ([t (unique-name 't)])
					(set! new-local* (cons t new-local*))
					t))
			(define (trivialize xpr)
				(let-values ([(code set!*) (simplify xpr)])
					(make-begin `(,@set!* ,code))))

			(define (simplify xpr)
				(match xpr
					[()  (values '() '())]
					[(,simple . ,[rem set!*])
					(guard (simple? simple))
					(values `(,simple ,rem ...) set!*)]
					[(,[Value -> v] . ,[rem set!*])
					(let ([t (new-t)])
						(values `(,t ,rem ...) `((set! ,t ,v) ,set!* ...)))]
					))
			(define (Value val)
				(match val
					[(if ,[Pred -> pred] ,[conseq] ,[altern]) `(if ,pred ,conseq ,altern)]
					[(begin ,[Effect -> ef*] ... ,[val]) (make-begin `(,ef* ... ,val))]
					[(alloc ,n) (trivialize `(alloc ,n))]
					[(mref ,b ,o) (trivialize `(mref ,b ,o))]
					[(,binop ,x ,y)
					(guard (memq binop '(+ - * logand logor sra)))
					(trivialize `(,binop ,x ,y))]
					[(,rator ,rand* ...) (trivialize `(,rator ,rand* ...))]
					[,tr (guard (triv? tr)) tr]
					))
			(define (Effect ef)
				(match ef
					[(nop) '(nop)]
					[(if ,[Pred -> pred] ,[conseq] ,[altern]) `(if ,pred ,conseq ,altern)]
					[(begin ,[ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
					[(mset! ,b ,o ,v) (trivialize `(mset! ,b ,o ,v))]
					[(set! ,var ,[Value -> val]) `(set! ,var ,val)]
					[(,rator ,rand* ...) (trivialize `(,rator ,rand* ...))]
					))
			(define (Pred pr)
				(match pr
					[(true) '(true)]
					[(false) '(false)]
					[(if ,[pred] ,[conseq] ,[altern]) `(if ,pred ,conseq ,altern)]
					[(begin ,[Effect -> ef*] ... ,[pr]) (make-begin `(,ef* ... ,pr))]
					[(,relop ,x ,y)
					(guard (memq relop '(< <= = >= >)))
					(trivialize `(,relop ,x ,y))]
					))
			(define (Tail tail)
				(match tail
					[(if ,[Pred -> pred] ,[conseq] ,[altern]) `(if ,pred ,conseq ,altern)]
					[(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,ef* ... ,tail))]
					[(alloc ,n) (trivialize `(alloc ,n))]
					[(mref ,b ,o) (trivialize `(mref ,b ,o))]
					[(,binop ,x ,y)
					(guard (memq binop '(+ - * logand logor sra)))
					(trivialize `(,binop ,x ,y))]
					[(,rator ,rand* ...) (trivialize `(,rator ,rand* ...))]
					[,tr (guard (triv? tr)) tr]
					))
			(match bd
				[(locals (,local* ...) ,[Tail -> tail])
				`(locals (,local* ... ,new-local* ...) ,tail)]
				))
(lambda (x)
	(match x
		[(letrec ([,label* (lambda (,fml** ...) ,[Body -> bd*])] ...)
			,[Body -> bd])
		`(letrec ([,label* (lambda (,fml** ...) ,bd*)] ...) ,bd)]
		)))
);end library