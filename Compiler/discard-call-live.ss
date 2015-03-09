(library (Compiler discard-call-live)
	(export discard-call-live)
	(import 
	  ;; Load Chez Scheme primitives:
	  (chezscheme)
	  ;; Load provided compiler framework:
	  (Framework driver)
	  (Framework wrappers)
	  (Framework match)
	  (Framework helpers)
	  (Framework prims))
	
	
	(define-who discard-call-live
		(define (Effect a)
			(match a
				[(nop) '(nop)]
				[(begin ,[ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
				[(if ,[Pred -> pred] ,[conseq] ,[alter])
				`(if ,pred ,conseq ,alter)]
				[(return-point ,label ,[Tail -> t]) `(return-point ,label ,t)]
				[(mset! ,base ,offset ,val) `(mset! ,base ,offset ,val)]
				[(set! ,var (mref ,base ,offset)) `(set! ,var (mref ,base ,offset))]
				[(set! ,x (,binop ,x ,y)) `(set! ,x (,binop ,x ,y))]
				[(set! ,x ,y) `(set! ,x ,y)]))
		(define (Pred a)
			(match a
				[(true) '(true)]
				[(false) '(false)]
				[(begin ,[Effect -> ef*] ... ,[pred])
				(make-begin `(,ef* ... ,pred))]
				[(if ,[pred] ,[conseq] ,[alter]) `(if ,pred ,conseq ,alter)]
				[(,relop ,x ,y) `(,relop ,x ,y)]))
		(define Tail
			(lambda (a)
				(match a
					[(if ,[Pred -> pred] ,[conseq] ,[alter]) `(if ,pred ,conseq ,alter)]
					[(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,ef* ... ,tail))]
					[(,triv ,loc* ...) `(,triv)])))
		(define Body
			(lambda (a)
				(match a
					[(locate ([,uvar* ,reg*] ...) ,[Tail -> tail])
					`(locate ([,uvar* ,reg*] ...) ,tail)])))
		(lambda (x)
			(match x
				[(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
				`(letrec ([,label* (lambda () ,body*)] ...) ,body)])))
)