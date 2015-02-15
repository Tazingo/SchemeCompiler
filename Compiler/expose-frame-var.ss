(library (Compiler expose-frame-var)
  (export expose-frame-var)
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load provided compiler framework:
    (Framework driver)
    (Framework wrappers)
    (Framework match)
    (Framework helpers))
    

    (define-who expose-frame-var
      (lambda(x)
	(match x
	       [,v 
		(guard (frame-var? v))
		(make-disp-opnd 'rbp (* 8 (frame-var->index v)))]
	       [(,v . ,r)
		`(,(expose-frame-var v) . ,(expose-frame-var r))]
	       [,e e])))
)