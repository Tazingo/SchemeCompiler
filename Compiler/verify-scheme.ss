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
	 Program  ->  (letrec ([label (lambda () Body)]*) Body)
	 Body     ->  (locals (uvar*) Tail)
	 Tail     ->  (Triv Loc*)
	          |   (if Pred Tail Tail)
	          |   (begin Effect* Tail)
	 Pred     ->  (true)
	          |   (false)
	          |   (relop Triv Triv)
	          |   (if Pred Pred Pred)
	          |   (begin Effect* Pred)
	 Effect   ->  (nop)
	          |   (set! Var Triv)
	          |   (set! Var (binop Triv Triv))
	          |   (if Pred Effect Effect)
	          |   (begin Effect* Effect)
	 Loc      ->  reg | fvar
	 Var      ->  uvar | Loc
	 Triv     ->  Var | int | label

	 |#
	 (define-who verify-scheme
	   ;; register?
	   ;; int32?
	   ;; int64?
	   ;; isbinop
	   ;; uvar?
	   
	   (define relop?
	     (lambda (x)
	       (memq x '(= < <= >= >))))

	   
	   (define (var? e)
	     (lambda(v)
	       (unless (or (register? v)
			   (frame-var? v)
			   (uvar? v))
		       (errorf who "invalid var ~s" v))
	       (when (uvar? v)
		     (unless (memq v e)
			     (errorf who "unbound uvar ~s" v)))
	       v))

	  
	   (define (binop? op)
	     (unless (isBinop op)
		     (errorf who "invalid op ~s" op))
	     op)

	   (define (loc? x)
	     (unless (or (register? x)
			 (frame-var? x))
		     (errorf who "invalid location ~s" x))
	     x)
	   
	   (define (triv? l e)
	     (lambda(t)
	       
	       (unless (or (register? t)
			   (label? t)
			   (frame-var? t)
			   (uvar? t)
			   (and (integer? t)
				(exact? t)))
		       (errorf who "invalid triv ~s" t))
	       (when(label? t)
		    (unless (memq t l)
			    (errorf who "unbound label ~s" t)))
	       (when(uvar? t)
		    (unless (memq t e)
			    (errorf who "unbound uvar ~s" t)))
	       t))
	   
	   (define (effect? l e)
	     (lambda(code)
	       (match code
		      
		      [(set! ,[(var? e) -> v1] (,[binop? -> op] ,[(triv? l e) -> v2] ,[(triv? l e) -> v3]))
		       (unless (and (eqv? v1 v2)
				    (case op

				      [(+ - logand logor)
				       (or (and (register? v1)
						(or (register? v3)
						    (frame-var? v3)
						    (int32? v3)
						    (uvar? v3)))
					   (and (frame-var? v1)
						(or (register? v3)
						    (int32? v3)
						    (uvar? v3)))
					   (uvar? v1))]
				      
				      [(*)
				       (or (and (register? v1)
						(or (register? v3)
						    (frame-var? v3)
						    (int32? v3)
						    (uvar? v3)))
					   (uvar? v1))]
				      
				      [(sra)
				       (and (or (register? v1)
						(frame-var? v1)
						(uvar? v1))
					    (uint6? v3))]

				      [else (errorf who "invalid binary operators ~s" op)]))

			       (errorf who "~s violates machine constraints" code))]
		      
		       [(set! ,[(var? e) -> v1] ,[(triv? l e) -> v2])
			(unless (or (and (register? v1)
					 (or (register? v2)
					     (frame-var? v2)
					     (int64? v2)
					     (label? v2)
					     (uvar? v2)))
				    (and (frame-var? v1)
					 (or (register? v2)
					     (int32? v2)
					     (uvar? v2)))
				    (uvar? v1))
				(errorf who "~s violates machine constraints" code))]
		       
		       [(if ,[(pred? l e) -> p] ,[e1] ,[e2])
			(void)]
		       [(begin ,[e*] ... ,[e])
			(void)]
		       [(nop)(void)]
		       [,x (errorf who "invalid effect ~s" x)])))
		      
	   (define (pred? l e)
	     (lambda(p)
	       (match p
		      [(true)(void)]
		      [(false)(void)]
		      [(begin ,[(effect? l e) -> e*] ... ,[t])
		       (void)]
		      [(if ,[p1] ,[p2] ,[p3])
		       (void)]
		      [(,relop ,[(triv? l e) -> v1] ,[(triv? l e) -> v2])
		       (guard (relop? relop))
		       (unless (or (and (or (register? v1)(uvar? v1))
					(or (register? v2)
					    (frame-var? v2)
					    (int32? v2)
					    (label? v2)
					    (uvar? v2)))
				   (and (frame-var? v1)
					(or (register? v2)
					    (uvar? v2)
					    (int32? v2))))
			       (errorf who "~s violates machine constraints" p))]
		      [,x (errorf who "invalid Pred ~s" x)])))

	   (define (tail? l e)
	     (lambda(t)
	       (match t
		      ;;(if p t1 t2)
		      [(if ,[(pred? l e) -> p] ,[(tail? l e) -> t1] ,[(tail? l e) -> t2])
		       (void)]
		      ;;(begin effect* tail)
		      [(begin ,[(effect? l e) -> exp] ... ,[(tail? l e) -> t])
		       (void)]
		      ;; (triv)
		      ;; For (triv) ,triv must not be a integer
		      [(,[(triv? l e) -> t] ,[loc? -> loc]...)
		       (when (integer? t)
			       (errorf who "~s violates machine constraints" t))]
		      
		      [,x (errorf who "invalid tail ~s" x)]
		      )))
	   
	   (define (body? l)
	     (lambda(b)
	       (match b
		      [(locals ,uvar ,t)
		       (verify-list uvar uvar? 'uvar)
		       ((tail? l uvar) t)]
		      [,x (errorf who "invalid body ~s" x)])))
	   
	   (define verify-list
	     (lambda(l x? x)
	       (letrec
		   ([check 
		     (lambda(l c)
		       (cond 
			[(null? l) (void)]
			[(not(x? (car l)))
			 (errorf who "invalid ~s ~s" x (car l))]
			[else (let ([a (extract-suffix (car l))])
				(if (member a c)
				    (errorf who "non-unique ~s suffix ~s" x a)
				    (check (cdr l)(cons a c))))]))])
		 (check l '()))))
				      
	       
	   (lambda(x)
	     (match x
		    [(letrec ([,l (lambda() ,t*)] ...) ,t)
		     (verify-list l label? 'label)
		     (for-each (body? l) t*)
		     ((body? l) t)]
		    [,x (errorf who "invalid program ~s" x)])
	     x))
	 )