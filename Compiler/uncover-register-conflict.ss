(library (Compiler uncover-register-conflict)
	(export uncover-register-conflict)
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

	(define-who uncover-register-conflict
		(lambda(x)
			(match x
				[(letrec ([,l (lambda(),[b*])]...) ,[b])
				`(letrec ([,l (lambda(),b*)]...) ,b)]
				
				[(locals (,local* ...)
					(ulocals (,ulocal* ...)
						(locate (,home* ...)
							(frame-conflict ,frame-conflict-graph ,tail))))
				(let*-values ([(graph call-live)(uncover-conflicts tail (union local* ulocal*) who register?)])
					`(locals (,local* ...)
						(ulocals (,ulocal* ...)
							(locate (,home* ...)
								(frame-conflict ,frame-conflict-graph
									(register-conflict ,graph ,tail))))))]
				
				[(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]		    
				)) 
		)
);end library