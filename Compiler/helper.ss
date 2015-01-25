(library (Compiler helper)
	 (export binop? binop-lookup var?)

	 (import 
	  ;; Load Chez Scheme primitives:
          (chezscheme)
	  ;; Load provided compiler framework:
	  (Framework driver)
	  (Framework wrappers)
	  (Framework match)
	  (Framework helpers)
	  (Framework prims))


	 ;;iff register
	 (define (var? v)
	   (if (or (register? v)
		   (frame-var? v))
	       v
	       (errorf "invalid var ~s" v)))

	 ;;iff binop
	 (define (binop? op)
	   (if (isBinop op)
	       op
	       (errorf "invalid op ~s" op)))

	 
	 ;; binop
	 (define (binop-lookup op)
	   (match op
		  [+ 'addq]
		  [- 'subq]
		  [* 'imulq]
		  [logand 'andq]
		  [logor 'orq]
		  [sra 'sraq]
		  [,x (errorf "invalid binop ~s" x)]))
)