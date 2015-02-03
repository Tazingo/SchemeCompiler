

(library (Compiler finalize-locations)
	 (export finalize-locations)
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
	 source language
	 Program  ->  (letrec ([label (lambda () Body)]*) Body)
	 Body     ->  (locate ([uvar Loc]*) Tail)
	 Tail     ->  (Triv)
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
	 ==============================
	 target language
	 Program  ->  (letrec ([label (lambda () Tail)]*) Tail)
	 Tail     ->  (Triv)
	          |   (if Pred Tail Tail)
	          |   (begin Effect* Tail)
	 Pred     ->  (true)
	          |   (false)
	          |   (relop Triv Triv)
	          |   (if Pred Pred Pred)
	          |   (begin Effect* Pred)
	 Effect   ->  (nop)
	          |   (set! Loc Triv)
	          |   (set! Loc (binop Triv Triv))
	          |   (if Pred Effect Effect)
	          |   (begin Effect* Effect)
	 Loc      ->  reg | fvar
	 Triv     ->  Loc | int | label
	 |#

	 (define-who finalize-locations
	   (define relop?
	     (lambda (x)
	       (memq x '(= < <= >= >))))

	   (define finalize
	     (lambda(x env)
	       (match x
		      [(letrec ([,l (lambda() ,[b*])] ...) ,[b])
		       `(letrec ([,l (lambda() ,b*)] ...) ,b)]
		      
		      [(locate ,env ,t)
		       (finalize t env)]
		     
		      [(,relop ,[t1] ,[t2])
		       (guard(or(isBinop relop)(relop? relop)))
		       `(,relop ,t1 ,t2)]
 
		      [,u
		       (guard (uvar? u))
		       (cadr (assq u env))]
		      
		     ; [(set! ,[v1] (,op ,[v2] ,[v3]))
		     ;  (guard (or (relop? op)(isBinop op)))
		     ;  `(set ,v1 (,op ,v2 ,v3))]
		      
		      [(set! ,[v] ,[t])
		       (if (eq? v t)
			   `(nop)
			   `(set! ,v ,t))]
		      
		      [(if ,[p] ,[t1] ,[t2])
		       `(if ,p ,t1 ,t2)] 
		      
		      [(begin ,[e*] ... ,[e])
		       `(begin ,e* ... ,e)]
		      
		     
		      
		      [,x x]
		      )))

	   (lambda(x)
	     (finalize x '())))
	 )