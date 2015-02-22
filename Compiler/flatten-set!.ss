(library (Compiler flatten-set!)
	(export flatten-set!)
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
	#|
	Program	->	(letrec ([label (lambda (uvar*) Body)]*) Body)
	Body	->	(locals (uvar*) Tail)
	Tail	->	Triv
	|	(binop Triv Triv)
	|	(Triv Triv*)
	|	(if Pred Tail Tail)
	|	(begin Effect* Tail)
	Pred	->	(true)
	|	(false)
	|	(relop Triv Triv)
	|	(if Pred Pred Pred)
	|	(begin Effect* Pred)
	Effect	->	(nop)
	|	(set! uvar Triv)
	|	(set! uvar (binop Triv Triv))
	|	(if Pred Effect Effect)
	|	(begin Effect* Effect)
	Triv	->	uvar | int | label
	|#

	(define-who (flatten-set! x)

		(define (flatten uvar value)
			(match value
				[(begin ,[Effect -> e*] ... ,[v]) (make-begin `(,e* ... ,v))]
				[(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
				[(,binop ,[Triv -> t] ,[Triv -> t^]) (guard (binop? binop)) `(set! ,uvar (,binop ,t ,t^))]
				[,t (guard (triv? t)) `(set! ,uvar ,t)]
				))

		(define (Triv t)
			(match t
				[,t^ (guard (triv? t^)) t^]
				))

		(define (Value v)
			(match v
				[(begin ,[Effect -> e*] ... ,[v^]) (make-begin `(,e* ... ,v^))]
				[(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
				[(,binop ,t ,t^) (guard (binop? binop)) `(,binop ,t ,t^)]
				[,t (guard (triv? t)) t]
				))

		(define (Effect e)
			(match e
				[(begin ,[Effect -> e*] ... ,[e^]) (make-begin `(,e* ... ,e^))]
				[(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
				[(nop) '(nop)]
				[(set! ,uvar ,v) (flatten uvar v)]
				))

		(define (Pred p)
			(match p
				[(true) '(true)]
				[(false) '(false)]
				[(begin ,[Effect -> e*] ... ,[p^]) (make-begin `(,e* ... ,p^))]
				[(if ,[p^] ,[c] ,[a]) `(if ,p^ ,c ,a)]
				[(,relop ,t ,t^) (guard (relop? relop)) `(,relop ,t ,t^)]
				))

		(define (Tail t)
			(match t
				[(begin ,[Effect -> e*] ... ,[t]) (make-begin `(,e* ... ,t))]
				[(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
				[(,binop ,t ,t^) (guard (binop? binop)) `(,binop ,t ,t^)]
				[(,[Triv -> t] ,[Triv -> t*] ...) `(,t ,t* ...)]
				[,t (guard (triv? t)) t]
				))


		(match x
			[(letrec ([,label* (lambda (,uvar* ...) ,[b*])] ...) ,[b])
			`(letrec ([,label* (lambda (,uvar* ...) ,b*) ] ...) ,b)]
			[(locals (,uvar* ...) ,[Tail -> t]) `(locals (,uvar* ...) ,t)]
			)

		)
);end library