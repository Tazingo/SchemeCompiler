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
				[(locals ,uvar* (new-frames ,frame* ,tail))
				(let*-values ([(graph call-live) (uncover-conflicts tail uvar* who frame-var?)]
					[(spills) (filter uvar? call-live)])
				`(locals ,uvar*
					(new-frames ,frame*
						(spills ,spills
							(frame-conflict ,graph
								(call-live ,call-live ,tail))))))]
				)))
);end library