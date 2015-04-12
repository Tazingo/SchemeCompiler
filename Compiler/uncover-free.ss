(library (Compiler uncover-free)
	(export uncover-free)
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
	(define-who uncover-free
		(define uncover
			(lambda (s)
				(cond
					[(null? s) '()]
					[else (cons (car s) (uncover (remq (car s) (cdr s))))])))

		(define (Expr ex)
			(match ex
				[(begin ,[ex* v**] ... ,[ex v*])
				(values (make-begin `(,ex* ... ,ex)) 
					(uncover `(,v** ... ... ,v* ...)))]
				[(if ,[pred pv*] ,[conseq cv*] ,[alter av*])
				(values `(if ,pred ,conseq ,alter) 
					(uncover `(,pv* ... ,cv* ... ,av* ...)))]
				[(let ([,uvar* ,[ex* v**]] ...) ,[ex v*])
				(let ([s (difference (uncover `(,v** ... ... ,v* ...)) uvar*)])
					(values `(let ([,uvar* ,ex*] ...) ,ex) s))]
				[(letrec ([,uvar* (lambda (,fml** ...) ,[ex* v**])] ...) ,[ex v*])
				(let ([x** (map difference v** fml**)])
					(let ([s (uncover (difference `(,x** ... ... ,v* ...) uvar*))])
						(values 
							`(letrec ([,uvar* (lambda (,fml** ...) 
								(free (,(map uncover x**) ...) ,ex*))] ...)
							,ex) s)))]
				[(quote ,imm) (values `(quote ,imm) '())]
				[(,prim ,[ex* v**] ...) (guard (prim? prim))
				(values `(,prim ,ex* ...) (uncover `(,v** ... ...)))]
				[(,[rator v*] ,[rand* v**] ...)
				(values `(,rator ,rand* ...) (uncover `(,v* ... ,v** ... ...)))]
				[,uvar (guard (uvar? uvar)) (values uvar `(,uvar))]
				))
		(lambda (x)
			(match x
				[,[Expr -> ex v*] 
				(if (null? v*) ex (error who "Unbound variables ~s" v*))])))
); end library