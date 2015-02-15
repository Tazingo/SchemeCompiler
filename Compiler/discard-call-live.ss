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
		(lambda (x)
			(match x
				[(letrec ([,label* (lambda () ,[body*])] ...) ,[body])
				`(letrec ([,label* (lambda () ,body*)] ...) ,body)]
				[(locate ([,uvar* ,reg*] ...) ,[tail])
				`(locate ([,uvar* ,reg*] ...) ,tail)]
				[(if ,p ,[c] ,[a]) `(if ,p ,c ,a)]
				[(begin ,e* ... ,[t]) `(begin ,e* ... ,t)] 
				[(,triv ,loc* ...) `(,triv)])))
	)