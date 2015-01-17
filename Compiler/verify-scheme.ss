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
	 Asssignment 1
	 Scheme Subset
	 Program -> (begin Statement+)
	 Statement -> (set! Var1 int64)
	 |  (set! Var1 Var2)
	 |  (set! Var1 (Binop Var1 int32))
	 |  (set! Var1 (Binop Var1 Var2))
	 Var -> rax | rcx | rdx | rbx | rbp | rsi | rdi
	 |  r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
	 Binop -> + | - | *
	 |#
	 (define-who (verify-scheme program)
	   ;; register?
	   ;; int32?
	   ;; int64?
	   ;; isbinop
	   
	   ;;iff register
	   (define (var? v)
	     (if (register? v)
		 v
		 (errorf who "invalid var ~s" v)))
	   ;;iff binop
	   (define (binop? op)
	     (if (isBinop op)
		 op
		 (errorf who "invalid op ~s" op)))
	   
	   (define (verify code)
	     (match code
		    ;; (set! Var1 (Binop Var1 int32))
		    [(set! ,[var? -> v1] (,[binop? -> op] ,[var? -> v2] ,n))
		     (guard (int32? n))
		     (if (eqv? v1 v2)
			 `(set! ,v1 (,op ,v2 ,n))
			 (errorf who "invalid syntax ~s" code))]
		    ;; (set! Var1 (Binop Var1 Var2))
		    [(set! ,[var? -> v1] (,[binop? -> op] ,[var? -> v2] ,[var? -> v3]))
		     (if (eqv? v1 v2)
			 `(set! ,v1 (,op ,v2 ,v3))
			 (errorf who "invalid syntax ~s" code))]
		    ;;(set! Var1 int64)
		    [(set! ,[var? -> v] ,n)
		     (guard (int64? n))
		     `(set! ,v ,n)]
		    ;;(set! Var1 Var2)
		    [(set! ,[var? -> v1] ,[var? -> v2])
		     `(set! ,v1 ,v2)]
		    
		    [,x (errorf who "invalid syntax ~s" x)]))
	   
	   (match program
		  [(begin ,[verify -> code] ,[verify -> code*] ...)
		   `(begin ,code ,code* ...)]
		  [,x (errorf who "invalid program ~s" x)])
	   )
	 )