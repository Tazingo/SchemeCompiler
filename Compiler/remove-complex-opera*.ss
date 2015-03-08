(library (Compiler remove-complex-opera*)
	(export remove-complex-opera*)
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
			|	(set! uvar Value)
			|	(if Pred Effect Effect)
			|	(begin Effect* Effect)
	Value	->	Triv
			|	(binop Triv Triv)
			|	(if Pred Value Value)
			|	(begin Effect* Value)
	Triv	->	uvar | int | label
	|#
	(define-who (remove-complex-opera* x)

		(define (Body b)

			(define new-local* '())

			(define (new-t)
				(let ([t (unique-name 't)])
					(set! new-local* (cons t new-local*))
					t))

			(define (simple? x)
				(or (and (integer? x) (exact? x))
					(label? x)
					(uvar? x)
					(binop? x)
					(relop? x)))

			(define (simp x)
				(let-values ([(code set!*) (ez x)])
					(make-begin `(,@set!* ,code))))

			(define (ez x)
				(match x
					[()  (values '() '())]
					[(,simple . ,[rem set!*])
					(guard (simple? simple))
					(values `(,simple ,rem ...) set!*)]
					[(,[Value -> v] . ,[rem set!*])
					(let ([t (new-t)])
						(values `(,t ,rem ...) `((set! ,t ,v) ,set!* ...)))]
					))

			(define (Value v)
				(match v
					[(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
					[(begin ,[Effect -> e*] ... ,[v^]) (make-begin `(,e* ... ,v^))]
					[(,binop ,v^ ,v&) (guard (binop? binop))
					(simp `(,binop ,v^ ,v&))]
					[,t (guard (triv? t)) t]
    				[(,[v] ,[v*] ...)(simp `(,v ,v* ...))]
					))

			(define (Effect e)
				(match e
					[(nop) e]
					[(set! ,uvar ,[Value -> v]) `(set! ,uvar ,v)]
					[(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
					[(begin ,[e*] ... ,[e]) (make-begin `(,e* ... ,e))]
     				[(,[Value -> v] ,[Value -> v*] ...)(simp `(,v ,v* ...))]
					))

			(define (Pred p)
				(match p
					[(true) p]
					[(false) p]
					[(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
					[(begin ,[Effect -> e*] ... ,[p]) (make-begin `(,e* ... ,p))]
					[(,relop ,v ,v^) (guard (relop? relop)) (simp `(,relop ,v ,v^))]
					))

			(define (Tail t)
				(match t
					[(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
					[(begin ,[Effect -> e*] ... ,[t]) (make-begin `(,e* ... ,t))]
					[(,binop ,v ,v^) (guard (binop? binop)) (simp `(,binop ,v ,v^))]
					[(,v ,v* ...) (simp `(,v ,v* ...))]
					[,t (guard (triv? t)) t]
					))

			(match b
				[(locals (,local* ...) ,[Tail -> t])
				`(locals (,local* ... ,new-local* ...) ,t)]
				))

		(match x
			[(letrec ([,label (lambda (,uvar* ...) ,[Body -> b*])] ...) ,[Body -> b])
			`(letrec ([,label (lambda (,uvar* ...) ,b*)] ...) ,b)]
			)
)
);end library