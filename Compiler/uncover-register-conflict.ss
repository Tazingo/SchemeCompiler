(library (Compiler uncover-register-conflict)
	 (export uncover-register-conflict)
	 (import 
	  ;; Load Chez Scheme primitives:
          (chezscheme)
	  ;; Load provided compiler framework:
	  (Framework driver)
	  (Framework wrappers)
	  (Framework match)
	  (Framework helpers)
	  (Framework prims))

	 (define-who uncover-register-conflict
	  
	   (define (graph-add! s conflict graph)
	     (when (uvar? s)
		   (let ([assoc (assq s graph)])
		     (if assoc
			 (set-cdr! assoc (set-cons conflict (cdr assoc)))
			 (cons (list s conflict) graph)
			 )))
	     graph
	     )

	   (define (update-graph s conflicts graph)
	     (let loop ([conflicts conflicts]
			[graph graph])
	       (cond
		[(null? conflicts) graph]
		[else (loop (cdr conflicts) (graph-add! 
					     (car conflicts) s (graph-add! s (car conflicts) graph)))])))
	   
	   (define (handle-var var ls)
	     (if (or (uvar? var) (register? var)) (set-cons var ls) ls))

	   (define (graph-union! g0 g1)
	     (for-each (lambda (assoc)
			 (let ([s (car assoc)]
			       [flicts (cdr assoc)])
			   (update-graph s flicts g1))) g0)
	     g1
	     )

	   (define (Effect effect* effect graph live)
	     (match effect
		    [(nop) (Effect* effect* graph live)]
		    [(set! ,lhs (,binop ,rhs0 ,rhs1))
		     (let ([ls (remove lhs live)])
		       (Effect* effect* (update-graph lhs ls graph) (handle-var rhs0 (handle-var rhs1 ls))))]
		    [(set! ,lhs ,rhs)
		     (let ([ls (remove lhs live)])
		       (Effect* effect* (update-graph lhs ls graph) (handle-var rhs ls)))]
		    [(if ,pred ,conseq ,altern)
		     (let*-values ([(ga lsa) (Effect '() altern graph live)]
				   [(gc lsc) (Effect '() conseq graph live)]
				   [(gp lsp) (Pred pred gc ga lsc lsa)])
		       (Effect* effect* gp lsp))]
		    [(begin ,e* ...) (Effect* (append effect* e*) graph live)]
		    ))

	   (define (Effect* effect* graph live)
	     (match effect*
		    [() (values graph live)]
		    [(,effect* ... ,effect) (Effect effect* effect graph live)]
		    ))

	   (define (Pred pred Cgraph Agraph Clive Alive)
	     (match pred
		    [(true) (values Cgraph Clive)]
		    [(false) (values Agraph Alive)]
		    [(begin ,effect* ... ,pred)
		     (let*-values ([(gp lsp) (Pred pred Cgraph Agraph Clive Alive)]
				   [(ge* lse*) (Effect* effect* gp lsp)])
		       (values ge* lse*))]
		    [(if ,pred ,conseq ,altern)
		     (let*-values ([(ga lsa) (Pred altern Cgraph Agraph Clive Alive)]
				   [(gc lsc) (Pred conseq Cgraph Agraph Clive Alive)]
				   [(gp lsp) (Pred pred gc ga lsc lsa)])
		       (values (graph-union! Cgraph Agraph) (union lsp lsc lsa)))]
		    [(,relop ,triv0 ,triv1)
		     (values (graph-union! Cgraph Agraph) (handle-var triv0 (handle-var triv1 (union Clive Alive))))]
		    ))

	   (define (Tail tail graph live)
	     (match tail
		    [(begin ,effect* ... ,tail)
		     (let*-values ([(gt lst) (Tail tail graph live)]
				   [(ge* lse*) (Effect* effect* gt lst)])
		       (values graph (union lst lse*)))]
		    [(if ,pred ,conseq ,altern)
		     (let*-values ([(ga lsa) (Tail altern graph live)]
				   [(gc lsc) (Tail conseq graph live)]
				   [(gp lsp) (Pred pred gc ga lsc lsa)])
		       (values graph (union lsp lsc lsa)))]
		    [(,triv ,loc* ...) (values graph (handle-var triv (union loc* live)))]
		    ))

	   (lambda(x)
	     (match x
		    [(letrec (,[block*] ...) ,[body])
		     `(letrec ,block* ,body)]
		    [(,label (lambda () ,[body])) `(,label (lambda () ,body))]
		    [(locals ,uvar* ,tail)
		     (let*-values ([(empty-graph) (map (lambda (s) (cons s '())) uvar*)]
				   [(live-set) '()]
				   [(graph lives) (Tail tail empty-graph live-set)])
		       `(locals ,uvar* (register-conflict ,graph ,tail)))]
		    
		    )) 
	 )
);end library