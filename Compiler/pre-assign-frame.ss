(library (Compiler pre-assign-frame)
	(export pre-assign-frame)
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



	(define-who pre-assign-frame
		(define (find-homes uvar* cgraph home*)
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
				(find-homes (remove uvar uvar*) cgraph `((,uvar ,home) . ,home*))
				)))
		(define Body
			(lambda (body)
				(match body
					[(locals (,local* ...)
           (new-frames (,frame* ...) ;; frame* is a set of sets
           	(spills (,spill* ...)
           		(frame-conflict ,fcg*
           			(call-live (,live* ...) ,tail)))))
					(let ([home* (find-homes spill* fcg* '())])
						`(locals (,local* ...)
							(new-frames (,frame* ...)
								(locate ,home*
									(frame-conflict ,fcg*
										(call-live (,live* ...) ,tail))))))])))
		(lambda (x)
			(match x
				[(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
				`(letrec ([,label* (lambda () ,body*)] ...) ,body)])))
);end library