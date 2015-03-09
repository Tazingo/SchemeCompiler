(library (Compiler expose-memory-operands)
	(export expose-memory-operands)
	(import
   	;; Load Chez Scheme primitives:
   	(chezscheme)
   	;; Load compiler framework:
   	(Framework match)
   	(Framework helpers)
   	(Compiler helper)
   	)

	(define-who expose-memory-operands
		(define (deref base offset)
			(if (register? offset)
				(make-index-opnd base offset)
				(make-disp-opnd base offset)))
		(define (Effect ef)
			(match ef
				[(nop) '(nop)]
				[(begin ,[ef*] ... ,[ef])
				(make-begin `(,ef* ... ,ef))]
				[(if ,[Pred -> pred] ,[conseq] ,[alter])
				`(if ,pred ,conseq ,alter)]
				[(mset! ,base ,offset ,val) `(set! ,(deref base offset) ,val)]
				[(return-point ,lbl ,[Tail -> tl]) `(return-point ,lbl ,tl)]
				[(set! ,var (mref ,base ,offset)) `(set! ,var ,(deref base offset))]
				[(set! ,x (,binop ,y ,z))
				`(set! ,x (,binop ,y ,z))]
				[(set! ,x ,y) `(set! ,x ,y)]))
		(define (Pred pred)
			(match pred
				[(true) '(true)]
				[(false) '(false)]
				[(begin ,[Effect -> ef*] ... ,[pred])
				(make-begin `(,ef* ... ,pred))]
				[(if ,[pred] ,[conseq] ,[alter])
				`(if ,pred ,conseq ,alter)]
				[(,relop ,x ,y) `(,relop ,x ,y)]
				[,x (error who "invalid Pred ~s" x)]))
		(define (Tail tail)
			(match tail
				[(begin ,[Effect -> ef*] ... ,[tail])
				(make-begin `(,ef* ... ,tail))]
				[(if ,[Pred -> pred] ,[conseq] ,[alter])
				`(if ,pred ,conseq ,alter)]
				[(,triv) `(,triv)]
				[,x (error who "invalid Tail ~s" x)]))
		(lambda (x)
			(match x
				[(letrec ([,label* (lambda () ,[Tail -> tail*])] ...) ,[Tail -> tail])
				`(letrec ([,label* (lambda () ,tail*)] ...) ,tail)]
				[,x (error who "invalid Tail ~s" x)])))
);