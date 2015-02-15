(library (Compiler introduce-allocation-forms)
	(export introduce-allocation-forms)
	(import
		(chezscheme)
		(Framework match)
		(Framework helpers))
	(define-who introduce-allocation-forms
		(lambda(x)
			(match x
				[(letrec ([,l (lambda() ,[b*])] ...) ,[b])
				`(letrec ([,l (lambda() ,b*)]...) ,b)]
				[(locals (,u ...) ,f)
				`(locals (,u ...)
					(ulocals ()
						(locate()
							,f)))]
				)))
 );end library