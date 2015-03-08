(library (Compiler assign-new-frame)
	(export assign-new-frame)
	(import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (Framework match)
   (Framework helpers)
   (Compiler helper)
   )
	(define-who (assign-new-frame program)

		(define (assign-frame frame* frame-index)
			(define (assign-nfv nfv* frame-index)
				(if (null? nfv*) '()
					(cons `(,(car nfv*) ,(index->frame-var frame-index))
						(assign-nfv (cdr nfv*) (add1 frame-index)))))
			(if (null? frame*) '()
				(append (assign-nfv (car frame*) frame-index) (assign-frame (cdr frame*) frame-index))))

		(define (Triv t)
			(match t
				[,t^ (guard (triv? t^)) t^]
				))

		(define (Effect frame-size)
			(lambda (e)
				(match e
					[(nop) '(nop)]
					[(begin ,[e*] ... ,[e^]) (make-begin `(,e* ... ,e^))]
					[(if ,[(Pred frame-size) -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
					[(return-point ,label ,[(Tail frame-size) -> tail])
					(make-begin
						`((set! ,frame-pointer-register
							(+ ,frame-pointer-register ,(ash frame-size word-shift)))
						(return-point ,label ,tail)
						(set! ,frame-pointer-register
							(- ,frame-pointer-register ,(ash frame-size word-shift)))))]
					[(set! ,uvar (,binop ,[Triv -> t] ,[Triv -> t^])) (guard (binop? binop))
					`(set! ,uvar (,binop ,t ,t^))]
					[(set! ,uvar ,[Triv -> t]) `(set! ,uvar ,t)]
					)))

		(define (Pred frame-size)
			(lambda (p)
				(match p
					[(true) '(true)]
					[(false) '(false)]
					[(begin ,[(Effect frame-size) -> e*] ... ,[p^]) (make-begin `(,e* ... ,p^))]
					[(if ,[p] ,[c] ,[a]) `(if ,p ,c ,a)]
					[(,relop ,[Triv -> t] ,[Triv -> t^]) (guard (relop? relop)) `(,relop ,t ,t^)]
					)))

		(define (Tail frame-size)
			(lambda (t)
				(match t
					[(begin ,[(Effect frame-size) -> e*] ... ,[t^]) (make-begin `(,e* ... ,t^))]
					[(if ,[(Pred frame-size) -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
					[(,binop ,[Triv -> t^] ,[Triv -> t&]) (guard (binop? binop)) `(,binop ,t^ ,t&)]
					[(,[Triv -> t^] ,[Triv -> t*] ...) `(,t^ ,t* ...)]
					[,t^ (guard (triv? t^)) t^]
					)))

		(define (Body b)
			(match b
				[(locals (,loc* ...)
					(new-frames (,frame* ...)
						(locate (,home* ...)
							(frame-conflict ,fgraph
								(call-live (,call-live ...)
									,t)))))
				(let* ([deref (lambda (v)
					(let ([home (assq v home*)])
						(cond
							[(frame-var? v) (frame-var->index v)]
							[home (frame-var->index (cadr home))]
							)))]
				[dereffed (map deref call-live)]
				[frame-size (add1 (if (null? dereffed) 0 (apply max dereffed)))])
				`(locals ,(difference loc* (apply append frame*))
					(ulocals ()
						(locate (,home* ... ,(assign-frame frame* frame-size) ...)
							(frame-conflict ,fgraph ,((Tail frame-size) t))))))]
				))

		(define (Program p)
			(match p
				[(letrec ([,label (lambda (,uvar* ...) ,[Body -> b*])] ...) ,[Body -> b])
				`(letrec ([,label (lambda (,uvar* ...) ,b*)] ...) ,b)]
				))

		(Program program)

)) ;; end library
