(library (Compiler uncover-frame-conflict)
	(export uncover-frame-conflict)
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

	(define-who uncover-frame-conflict
		(lambda(x)
			(match x
				[(letrec ([,l (lambda() ,[b*])] ...),[b])
				`(letrec ([,l (lambda() ,b*)] ...),b)]
				[(locals ,u ,tail)
				`(locals ,u
					(frame-conflict ,(do-uncover-conflict tail u who frame-var?) ,tail))])
			)
		)
);end library