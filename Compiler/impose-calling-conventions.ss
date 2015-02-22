(library (Compiler impose-calling-conventions)
	(export impose-calling-conventions)
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

	Program	->	(letrec ([label (lambda () Body)]*) Body)
	Body	->	(locals (uvar*) Tail)
	Tail	->	(Triv Loc*)
	|	(if Pred Tail Tail)
	|	(begin Effect* Tail)
	Pred	->	(true)
	|	(false)
	|	(relop Triv Triv)
	|	(if Pred Pred Pred)
	|	(begin Effect* Pred)
	Effect	->	(nop)
	|	(set! Var Triv)
	|	(set! Var (binop Triv Triv))
	|	(if Pred Effect Effect)
	|	(begin Effect* Effect)
	Loc	->	reg | fvar
	Var	->	uvar | Loc
	Triv	->	Var | int | label
	|#

	(define-who impose-calling-conventions
		(define param-locations
			(lambda (p)
				(let gen ([p p] [preg parameter-registers] [fv-index 0])
					(cond
						[(null? p) '()]
						[(null? preg) (cons (index->frame-var fv-index)
							(gen (cdr p) preg (add1 fv-index)))]
						[else (cons (car preg) (gen (cdr p) (cdr preg) fv-index))]))))
		(define Tail
			(lambda (rp)
				(lambda (tail)
					(match tail
						[(begin ,ef* ... ,[tail])
						(make-begin `(,ef* ... ,tail))]
						[(if ,pred ,[conseq] ,[alter])
						`(if ,pred ,conseq ,alter)]
						[(,binop ,triv1 ,triv2) (guard (binop? binop))
						(make-begin `((set! ,return-value-register (,binop ,triv1 ,triv2))
							(,rp ,frame-pointer-register ,return-value-register)))]
						[(,triv ,triv* ...)
						(let ([arg-loc* (param-locations triv*)])
							(make-begin
								`((set! ,(reverse arg-loc*) ,(reverse triv*)) ... 
									(set! ,return-address-register ,rp)
									(,triv ,frame-pointer-register ,return-address-register ,arg-loc* ...))))]
						[,triv (guard triv? triv)
						(make-begin `((set! ,return-value-register ,triv)
							(,rp ,frame-pointer-register ,return-value-register)))]))))
		(define Body
			(lambda (body p)
				(match body
					[(locals (,local* ...) ,tail)
					(let ([rp (unique-name 'rp)]
						[param-loc* (param-locations p)])
					(let ([tail ((Tail rp) tail)])             
						`(locals (,local* ... ,rp ,p ...)
							,(make-begin
								`((set! ,rp ,return-address-register)
									(set! ,p ,param-loc*) ... ,tail)))))])))
		(lambda (x)
			(match x
				[(letrec ([,label* (lambda (,p* ...) ,body*)] ...) ,body)
				`(letrec ([,label* (lambda () ,(map Body body* p*))] ...) ,(Body body '()))])))

)