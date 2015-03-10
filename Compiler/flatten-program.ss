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
	     
	     [(if ,p (,c) (,a))
	      (cond [(eq? c #f)
		     `((if (not ,p) (jump ,a)))]
		    [(eq? a #f)
		     `((if ,p (jump ,c)))]
		    [else `((if ,p (jump ,c)) (jump ,a))])]
	     
	     [(,t) `((jump ,t))]
	     
	     [,x x]))
)