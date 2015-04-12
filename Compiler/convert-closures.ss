(library (Compiler convert-closures)
	(export convert-closures)
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
	(define-who convert-closures
		(lambda (ex)
			(match ex
				[(begin ,[ex*] ... ,[ex])
				(make-begin `(,ex* ... ,ex))]
				[(if ,[pred] ,[conseq] ,[alter])
				`(if ,pred ,conseq ,alter)]
				[(let ([,uvar* ,[ex*]] ...) ,[ex])
				`(let ([,uvar* ,ex*] ...) ,ex)]
				[(letrec ([,uvar* (lambda (,fml** ...) (free (,v** ...) ,[ex*]))] ...) ,[ex])
				(let ([lb* (map unique-label uvar*)]
					[cp* (map (lambda (uvar) (unique-name 'cp)) uvar*)])
				`(letrec ([,lb* (lambda (,cp* ,fml** ...)
					(bind-free (,cp* ,v** ...)
						,ex*))] ...)
				(closures ([,uvar* ,lb* ,v** ...] ...)
					,ex)))]
				[(quote ,imm) `(quote ,imm)]
				[(,prim ,[ex*] ...) (guard (prim? prim))
				`(,prim ,ex* ...)]
				[(,[rator] ,[rand*] ...)
				(if (uvar? rator)
					`(,rator ,rator ,rand* ...)
					(let ([tmp (unique-name 'tmp)])
						`(let ([,tmp ,rator]) (,tmp ,tmp ,rand* ...))))]
				[,uvar (guard (uvar? uvar)) uvar]
				)))
 );end library