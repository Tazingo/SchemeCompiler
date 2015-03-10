(library (Compiler remove-let)
	(export remove-let)
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

	(define-who remove-let
		(lambda (x)
			(define rem1
				(lambda (x)
					(make-begin (rem x))))
			(define rem
				(lambda (x)
					(match x
						[(letrec ((,label* (lambda (,uvar* ...)
							(locals (,local* ...) ,[rem1 -> body*]))) ...)
						(locals (,local ...) ,[rem1 -> body]))
						`(letrec ((,label* (lambda (,uvar* ...)
							(locals (,local* ...) ,body*))) ...)
						(locals (,local ...) ,body))]
						[(begin ,[s*] ...) `((begin ,s* ... ...))]
						[(let ((,x* ,[v*]) ...) ,[body])
      					`(,(make-begin `((set! ,x* ,@v*) ... ,@body)))]
						[(if ,[test] ,[conseq] ,[alt])
						`((if ,@test ,@conseq ,@alt))]
						[(alloc ,[n]) `((alloc ,@n))]
						[(mset! ,[base] ,[off] ,[val]) `((mset! ,@base ,@off ,@val))]
						[(set! ,x ,[y]) `((set! ,x ,@y))]
						[(,[f] ,[a*] ...) `((,@f ,a* ... ...))]
						[,other `(,other)])))
			(rem x)))

 );end library