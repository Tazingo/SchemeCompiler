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
	  (Framework prims))

	 (define-who assign-registers


	   (define (graph-remove x graph)
	     (let ([graph (map (lambda (y) (cons (car y) (remove x (cdr y)))) graph)])
	       (remove (assq x graph) graph)))
	   
	   
	   (define (low-degree cur uvar* conflict*)
	     (cond
	      [(null? uvar*) cur]
	      [(< (length registers) (length (assq cur conflict*)))
	       (low-degree (car uvar*) (cdr uvar*) conflict*)]
	      [else (low-degree cur (cdr uvar*) conflict*)]))
	   
	   (define (play-nice uvar* conflict*)
	     (if (null? uvar*) '()
		 (let* ([uvar (low-degree (car uvar*) (cdr uvar*) conflict*)] ;; pick low degree variable
			[flict (assq uvar conflict*)]
			[alist (play-nice (remove uvar uvar*) (graph-remove uvar conflict*))])
		   `((,uvar ,(pick-reg uvar flict alist)) . ,alist))))
	   
	   
	   (define (pick-reg uvar conflicts alist)
	     (let* ([conflicts (map (lambda (x) (if (assq x alist) (cadr (assq x alist)) x)) conflicts)]
		    [avail (difference registers conflicts)])
	       (if (null? avail)
		   (errorf 'assign-registers "out of registers (~s) for ~s but built ~s" avail uvar alist)
		   (car avail))))
	   
	   (lambda(x)
	     (match x
		    [(letrec ([,l (lambda() ,[b*])]...) ,[b])
		     `(letrec ([,l (lambda() ,b*)]...) ,b)]
		    [(locals ,uvar* (register-conflict ,graph ,tail))
		     `(locate ,(play-nice uvar* graph) ,tail)]
		    )))
	 )