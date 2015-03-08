(library (Compiler assign-registers)
	 (export assign-registers)
	 (import 
	  ;; Load Chez Scheme primitives:
          (chezscheme)
	  ;; Load provided compiler framework:
	  (Framework driver)
	  (Framework wrappers)
	  (Framework match)
	  (Framework helpers)
	  (Framework prims)
   	  (Compiler helper))

	 (define-who assign-registers
	   	   
	   (define (low-degree cur uvar* conflict*)
	     (cond
	      [(null? uvar*) cur]
	      [(< (length registers) (length (assq cur conflict*)))
	       (low-degree (car uvar*) (cdr uvar*) conflict*)]
	      [else (low-degree cur (cdr uvar*) conflict*)]))
	   
	   (define (play-nice uvar* conflict* spills)
	   	(if (null? uvar*) (values '() spills)
	   		(let*-values
            ([(uvar) (low-degree (car uvar*) (cdr uvar*) conflict*)] ;; pick low degree variable
            	[(alist spills) (play-nice (remove uvar uvar*) (graph-remove uvar conflict*) spills)]
            	[(conflicts)
            	(map (lambda (x)
            		(if (assq x alist) (cadr (assq x alist)) x))
            	(assq uvar conflict*))]
            	[(avail) (difference registers conflicts)])
            (if (null? avail) (values alist (set-cons uvar spills))
            	(values `((,uvar ,(car avail)) . ,alist) spills)))))
	   	   
	   (lambda(x)
	     (match x
		    [(letrec ([,l (lambda() ,[b*])]...) ,[b])
		    `(letrec ([,l (lambda() ,b*)]...) ,b)]
		    [(locals (,locals ...)
		    	(ulocals (,ulocals ...)
		    		(locate ,locate
		    			(frame-conflict ,fgraph
               (register-conflict ,rgraph ,tail))))) ;; <--- LHS || RHS ---V

		    (let-values ([(bind* spills) (play-nice (union ulocals locals) rgraph '())])
		    	(if (null? spills) `(locate ,bind* ,tail)
		    		`(locals ,(difference locals spills)
		    			(ulocals ,(difference ulocals spills)
		    				(spills ,spills
		    					(locate ,locate
		    						(frame-conflict ,fgraph ,tail)))))))]
		    [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
		    )))
	 )