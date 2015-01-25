(library (Compiler verify-scheme)
	 (export verify-scheme)
	 (import 
	  ;; Load Chez Scheme primitives:
          (chezscheme)
	  ;; Load provided compiler framework:
	  (Framework driver)
	  (Framework wrappers)
	  (Framework match)
	  (Framework helpers)
	  (Framework prims))
	 
	 #|
	 Assignment 2
	 Program -> (letrec ([label (lambda () Tail)]*) Tail)
	 Tail    -> (Triv)
	         |  (begin Effect* Tail)
	 Effect  -> (set! Var Triv)
	         |  (set! Var (binop Triv Triv))
	 Var     -> reg | fvar
	 Triv    -> Var | int | label
	 |#
	 (define-who verify-scheme
	   ;; register?
	   ;; int32?
	   ;; int64?
	   ;; isbinop
	   ;; uvar?
	   
	   ;;iff register
	   (define (var? v)
	     (if (or (register? v)
		     (frame-var? v))
		 v
		 (errorf who "invalid var ~s" v)))

	   ;;iff binop
	   (define (binop? op)
	     (if (isBinop op)
		 op
		 (errorf who "invalid op ~s" op)))


	   
	   (define (triv? l)
	     (lambda(t)
	       (unless (or (register? t)
			   (label? t)
			   (frame-var? t)
			   (and (integer? t)
				(exact? t)))
		       (errorf who "invalid triv ~s" t))
	       (when(label? t)
		    (unless (memq t l)
			    (error who "unbound label ~s" t)))
	       t))
	   
	   (define (effect? l)
	     (lambda(code)
	       (match code
		      #| 
		      (set! Var1 (Binop Var1 int32))
		      In (set! Var (op Triv1 Triv2)), Triv1 must be identical to Var.
		      In (set! Var (op Triv1 Triv2)), Triv1 and Triv2 cannot both be frame variables.
		      For (set! Var (* Triv Triv)), Var must be a register.
		      For (set! Var (sra Triv1 Triv2)), Triv2 must be an exact integer k,k -> uint6 
		      |#
		      [(set! ,[var? -> v1] (,[binop? -> op] ,[(triv? l) -> v2] ,[(triv? l) -> v3]))
		       (unless (and (eqv? v1 v2)
				    (case op

				      [(+ - logand logor)
				       (or (and (register? v1)
						(or (register? v3)
						    (frame-var? v3)
						    (int32? v3)))
					   (and (frame-var? v1)
						(or (register? v3)
						    (int32? v3))))]
				      
				      [(*)
				       (or (and (register? v1)
						(or (register? v3)
						    (frame-var? v3)
						    (int32? v3))))]
				      
				      [(sra)
				       (and (or (register? v1)
						(frame-var? v1))
					    (uint6? v3))]

				      [else (errorf who "invalid binary operators ~s" op)]))

			       (errorf who "~s violates machine constraints" code))]
		       
		       #|
		      (set! Var1 Var2)
		      In (set! Var Triv), Var and Triv cannot both be frame variables.	
		      In (set! Var Triv), if Triv is a label, Var must be a register. 
		      (This is because we need to use the leaq instruction, which requires the destination to be a register.)
		      In (set! Var Triv), if Triv is an integer n, either 
		          a) n is int32 or
		          b) Var must be a register and n is int64 
		       |#
		       [(set! ,[var? -> v1] ,[(triv? l) -> v2])
			(unless (or (and (register? v1)
					 (or (register? v2)
					     (frame-var? v2)
					     (int64? v2)
					     (label? v2)))
				    (and (frame-var? v1)
					 (or (register? v2)
					     (int32? v2))))
				(errorf who "~s violates machine constraints" code))]
		      
			[,x (errorf who "invalid effect ~s" x)])))
		      
	   (define (tail? l)
	     (lambda(t)
	       (match t
		      ;;(begin effect* tail)
		      [(begin ,[(effect? l) -> exp] ... ,t)
		       ((tail? l) t)]
		      ;; (triv)
		      ;; For (triv) ,triv must not be a integer
		      [(,[(triv? l) -> t])
		       (when (integer? t)
			       (errorf who "~s violates machine constraints" t))]
		      
		      [,x (errorf who "invalid tail ~s" x)]
		      )))

	   (define verify-label
	     (lambda(l)
	       (letrec
		   ([check 
		     (lambda(l c)
		       (cond 
			[(null? l) (void)]
			[(not(label? (car l)))
			 (errorf who "invalid label ~s" (car l))]
			[else (let ([x (extract-suffix (car l))])
				(if (member x c)
				    (errorf who "non-unique label suffix ~s" x)
				    (check (cdr l)(cons x c))))]))])
		 (check l '()))))
				      
	       
	   (lambda(x)
	     (match x
		    [(letrec ([,l (lambda() ,t*)]...) ,t)
		     (verify-label l)
		     (for-each (tail? l) t*)
		     ((tail? l) t)]
		    [,x (errorf who "invalid program ~s" x)])
	     x))
	 )