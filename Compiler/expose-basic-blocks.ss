(library (Compiler expose-basic-blocks)
	 (export expose-basic-blocks)
	 (import 
	  ;; Load Chez Scheme primitives:
          (chezscheme)
	  ;; Load provided compiler framework:
	  (Framework driver)
	  (Framework wrappers)
	  (Framework match)
	  (Framework helpers)
	  (Framework prims))
	
	 #|(define-who (expose-basic-blocks program)
	   (define relop?
	     (lambda (x)
	       (memq x '(= < <= >= >))))

	   (define (expose-tail tail)
	     (match tail
		    [(if ,pred ,tail0 ,tail1)
		     (let ([clbl0 (unique-label 'c)]
			   [albl0 (unique-label 'a)])
		       (let-values ([(pexpr pbinds) (expose-pred pred clbl0 albl0)]
				    [(cexpr cbinds) (expose-tail tail0)]
				    [(aexpr abinds) (expose-tail tail1)])
			 (values pexpr `(,pbinds ...
						 [,clbl0 (lambda () ,cexpr)]
						 ,cbinds ...
						 [,albl0 (lambda () ,aexpr)]
						 ,abinds ...
						 ))))]
		    [(begin ,effect* ... ,tail)
		     (let*-values ([(texpr tbinds) (expose-tail tail)]
				   [(eexpr ebinds) (expose-effect* effect* `(,texpr))])
		       (values eexpr `(,ebinds ... ,tbinds ...)))]
		    [(,triv)
		     (values `(,triv) '())]
		    [,x (errorf who "unexpected tail: ~s" x) x]
		    )
	     )
	   (define (expose-pred pred clbl albl)
	     (match pred
		    [(true)
		     (values `(,clbl) '())]
		    [(false)
		     (values `(,albl) '())]
		    [(,relop ,r0 ,r1) (guard (relop? relop))
		     (values `(if (,relop ,r0 ,r1) (,clbl) (,albl)) '())]
		    [(if ,pred ,conseq ,altern)
		     (let ([clbl0 (unique-label 'c)]
			   [albl0 (unique-label 'a)])
		       (let-values ([(pexpr pbinds) (expose-pred pred clbl0 albl0)]
				    [(cexpr cbinds) (expose-pred conseq clbl albl)]
				    [(aexpr abinds) (expose-pred altern clbl albl)])
			 (values pexpr
				 `(,pbinds ...
					   [,clbl0 (lambda () ,cexpr)]
					   ,cbinds ...
					   [,albl0 (lambda () ,aexpr)]
					   ,abinds ...
					   ))))]
		    [(begin ,effect* ... ,pred)
		     (let*-values ([(pexp pbinds) (expose-pred pred clbl albl)]
				   [(eexp ebinds) (expose-effect* effect* `(,pexp))])
		       (values eexp `(,ebinds ... ,pbinds ...)))]
		    [,x (errorf who "unexpected pred: ~s" x) x]
		    )
	     )
	   (define (expose-effect* effect* acc)
	     (match effect*
		    [() (values (make-begin acc) '())]
		    [(,e* ... ,e) (expose-effect e* e acc)]
		    [,x (errorf who "unexpected effect*: ~s" x) x]
		    )
	     )
	   (define (expose-effect effect* effect acc)
	     (match effect
		    [(nop) (expose-effect* effect* acc)]
		    [(set! . ,x) (expose-effect* effect* `((set! . ,x) ,acc ...))]
		    [(if ,pred ,conseq ,altern)
		     (let ([clbl (unique-label 'c)]
			   [albl (unique-label 'a)]
			   [jlbl (unique-label 'j)])
		       (let*-values ([(pexpr pbinds) (expose-pred pred clbl albl)]
				     [(cexpr cbinds) (expose-effect '() conseq `((,jlbl)))]
				     [(aexpr abinds) (expose-effect '() altern `((,jlbl)))]
				     [(eexpr ebinds) (expose-effect* effect* `(,pexpr))])
			 (values eexpr
				 `(,ebinds ...
					   ,pbinds ...
					   [,clbl (lambda () ,cexpr)]
					   ,cbinds ...
					   [,albl (lambda () ,aexpr)]
					   ,abinds ...
					   [,jlbl (lambda () ,(make-begin acc))])
				 )))]
		    [(begin ,e* ...) (expose-effect* (append effect* e*) acc)]
		    [,x (errorf who "unexpected effect: ~s" x) x]
		    )
	     )
	   
	     (match program
		    [(letrec ([,label* (lambda () ,[expose-tail -> texp* tbinds*])] ...) ,[expose-tail -> texp tbinds])
		     `(letrec ([,label* (lambda () ,texp*)] ... ,tbinds* ... ... ,tbinds ...) ,texp)]
		    [,x (errorf who "unexpected program: ~s" x) x]
		    )
	 )|#
	 (define-who (expose-basic-blocks program)
	   (define relop?
	     (lambda (x)
	       (memq x '(= < <= >= >))))

	   (define (expose-pred p $c $a)
	     (match p
		    [(true)
		     (values `(,$c) '())]
		    [(false)
		     (values `(,$a) '())]
		    [(,relop ,r0 ,r1) (guard (relop? relop))
		     (values `(if (,relop ,r0 ,r1) (,$c) (,$a)) '())]
		    [(if ,p ,c ,a)
		     (let ([$c0 (unique-label 'c)]
			   [$a0 (unique-label 'a)])
		       (let-values ([(pe pb) (expose-pred p $c0 $a0)]
				    [(ce cb) (expose-pred c $c $a)]
				    [(ae ab) (expose-pred a $c $a)])
			 (values pe
				 `(,pb ...
				       [,$c0 (lambda () ,ce)]
				       ,cb ...
				       [,$a0 (lambda () ,ae)]
				       ,ab ...
				       ))))]
		    [(begin ,effect* ... ,p)
		     (let*-values ([(pexp pb) (expose-pred p $c $a)]
				   [(eexp ebinds) (expose-effect* effect* `(,pexp))])
		       (values eexp `(,ebinds ... ,pb ...)))]
		    )
	     )
	   (define (expose-effect* effect* acc)
	     (match effect*
		    [() (values (make-begin acc) '())]
		    [(,e* ... ,e) (expose-effect e* e acc)]
		    )
	     )
	   (define (expose-effect effect* effect acc)
	     (match effect
		    [(nop) (expose-effect* effect* acc)]
		    [(set! . ,x) (expose-effect* effect* `((set! . ,x) ,acc ...))]
		    [(if ,p ,c ,a)
		     (let ([$c (unique-label 'c)]
			   [$a (unique-label 'a)]
			   [$j (unique-label 'j)])
		       (let*-values ([(pe pb) (expose-pred p $c $a)]
				     [(ce cb) (expose-effect '() c `((,$j)))]
				     [(ae ab) (expose-effect '() a `((,$j)))]
				     [(eexpr ebinds) (expose-effect* effect* `(,pe))])
			 (values eexpr
				 `(,ebinds ...
					   ,pb ...
					   [,$c (lambda () ,ce)]
					   ,cb ...
					   [,$a (lambda () ,ae)]
					   ,ab ...
					   [,$j (lambda () ,(make-begin acc))])
				 )))]
		    [(begin ,e* ...) (expose-effect* (append effect* e*) acc)]
		    )
	     )
	   (define (expose-tail tail)
	     (match tail
		    [(if ,p ,t1 ,t2)
		     (let ([$c0 (unique-label 'c)]
			   [$a0 (unique-label 'a)])
		       (let-values ([(pe pb) (expose-pred p $c0 $a0)]
				    [(ce cb) (expose-tail t1)]
				    [(ae ab) (expose-tail t2)])
			 (values pe `(,pb ...
					  [,$c0 (lambda () ,ce)]
					  ,cb ...
					  [,$a0 (lambda () ,ae)]
					  ,ab ...
					  ))))]
		    [(begin ,effect* ... ,tail)
		     (let*-values ([(texpr tbinds) (expose-tail tail)]
				   [(eexpr ebinds) (expose-effect* effect* `(,texpr))])
		       (values eexpr `(,ebinds ... ,tbinds ...)))]
		    [(,triv)
		     (values `(,triv) '())]
		    )
	     )

	   
	   (match program
		  [(letrec ([,l (lambda () ,[expose-tail -> te* tb*])] ...) ,[expose-tail -> te tb])
		   `(letrec ([,l (lambda () ,te*)] ... ,tb* ... ... ,tb ...) ,te)]
		  )
	   
	   
	   )
)