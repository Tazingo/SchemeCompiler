(library (Compiler flatten-program)
  (export flatten-program)
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load provided compiler framework:
    (Framework driver)
    (Framework wrappers)
    (Framework match)
    (Framework helpers))
    

    (define-who (flatten-program code)
      (match code
	     [(letrec(,[l] ...) ,[t])
	      `(code ,t ... ,l ... ...)]

	     [(begin ,e ... ,[t])
	      `(,e ... ,t ...)]

	     [(,l (lambda() ,[b]))
	      `(,l ,b ...)]
	     
	     [(,t) `((jump ,t))]
	     
	     [,x x]))
)