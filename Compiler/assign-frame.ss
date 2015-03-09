(library (Compiler assign-frame)
	(export assign-frame)
	(import 
	  ;; Load Chez Scheme primitives:
	  (chezscheme)
	  ;; Load provided compiler framework:
	  (Framework driver)
	  (Framework wrappers)
	  (Framework match)
	  (Framework helpers)
	  (Framework prims))

	(define-who assign-frame

		(define (play-nice uvar* cgraph home*)
			(if (null? uvar*) home*
				(let* ([uvar (car uvar*)]
					[conflict* (assq uvar cgraph)]
					[used (let find-used ([conflict* (cdr conflict*)])
						(cond
							[(null? conflict*) '()]
							[(frame-var? (car conflict*))
							(set-cons (car conflict*) (find-used (cdr conflict*)))]
							[(assq (car conflict*) home*) =>
							(lambda (x) (set-cons (cadr x) (find-used (cdr conflict*))))]
							[else (find-used (cdr conflict*))]))]
					[home (let find-home ([index 0])
						(let ([fv (index->frame-var index)])
							(if (memq fv used) (find-home (add1 index)) fv)))])
				(play-nice (remove uvar uvar*) cgraph `((,uvar ,home) . ,home*))
				)))

		(lambda(x)
			(match x
				[(letrec ([,l (lambda() ,[b*])]...) ,[b])
				`(letrec ([,l (lambda() ,b*)]...) ,b)]
				[(locals (,local* ...)
					(ulocals (,ulocal* ...)
						(spills (,spill* ...)
							(locate (,home* ...)
								(frame-conflict ,fgraph ,tail)))))
				
				(let ([bind* (play-nice spill* fgraph home*)])
					`(locals ,(difference local* spill*)
						(ulocals ,(difference ulocal* spill*)
							(locate (,bind* ...)
								(frame-conflict ,fgraph ,tail)))))]
				[(locate (,home* ...) ,tail)  `(locate (,home* ...) ,tail)]
				)))

); end library