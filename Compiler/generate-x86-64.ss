(library (Compiler generate-x86-64)
	 (export generate-x86-64)
	 (import
	  (chezscheme)
	  (Framework driver)
	  (Framework wrappers)
	  (Framework match)
	  (Framework helpers))

	 (define-who (generate-x86-64 exp)
	   
	   ;; binop
	   (define (binop-lookup op)
	     (match op
		    [+ 'addq]
		    [- 'subq]
		    [* 'imulq]
		    [logand 'andq]
		    [logor 'orq]
		    [sra 'sarq]
		    [,x (errorf "invalid binop ~s" x)]))

	   
	   (define (gene code) 
	     (match code
		    ;; (set! dst (binop dst src))
		    
		    [(set! ,dst (,op ,dst ,src))
		     (emit (binop-lookup op) src dst)]

		    [(set! ,dst ,src)
		     (if (label? src)
			 (emit 'leaq src dst)
			 (emit 'movq src dst))]
		    
		    [(jump ,x)
		     (emit-jump 'jmp x)]
		    [,x (guard (label? x))
			(emit-label x)]
		    
		    [,x (errorf who "invalid syntax ~s" x)]))

	   (match exp
		  [(code ,exp ...)(emit-program (for-each gene exp))]
		  [,x (errorf who "invalid program ~s" x)]
		  )
	   )
	 )