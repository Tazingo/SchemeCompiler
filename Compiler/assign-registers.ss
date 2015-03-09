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
		(define select-indirect
			(lambda (uvar* home*)
				(let f ([uvar* uvar*][indirect* '()])
					(cond
						[(null? uvar*) indirect*]
						[else (let ([p (assq (car uvar*) home*)])
							(f (cdr uvar*) (if p (cons (cadr p) indirect*) indirect*)))]))))
		(define reglen (lambda () (length registers)))
		(define pick-if
			(lambda (pred? cg)
				(cond
					[(null? cg) #f]
					[(pred? (car cg)) (car cg)]
					[else (pick-if pred? (cdr cg))])))
		(define low-degree?
			(lambda (row)
				(< (sub1 (length row)) (reglen))))
		(define pick-one
			(lambda (cg ulocal*)
				(let* ([spillable? (lambda (row)
					(not (memq (car row) ulocal*)))]
				[pick (or (pick-if low-degree? cg) (pick-if spillable? cg))])
				(if pick pick (error  who "Only high-degree unspillables are left")))))
		(define remove-conflicts!
			(lambda (pick cg)
				(for-each (lambda (row)
					(set-cdr! row (remq (car pick) (cdr row)))) cg)))
		(define simplify-and-select
    (lambda (uvar* ulocal* cg) ;; uvar* contains locals and ulocal* (unspillables)
    	(if (null? uvar*)
          (values '() '()) ; if uvar* is null no assignments & spills
          (let* ([pick (pick-one cg ulocal*)] [cg (remq pick cg)])
          	(remove-conflicts! pick cg)
          	(let*-values
          		([(alist spills) (simplify-and-select (remq (car pick) uvar*) ulocal* cg)]
                 [(pickregs) (filter register? (cdr pick))] ; registers in picked row
                 [(otherregs)  (select-indirect (difference (cdr pick) pickregs) alist)]
                 [(availregs) (difference registers (union pickregs otherregs))])
          		(if (not (null? availregs))
          			(values
          				(cons `(,(car pick) ,(car availregs)) alist)
          				spills)
          			(values alist (cons (car pick) spills))))))))
		(define Body
			(lambda (body)
				(match body
					[(locals (,local* ...)
						(ulocals (,ulocal* ...)
							(locate ([,uvar* ,fvar*] ...)
								(frame-conflict ,fcg*
									(register-conflict ,rcg* ,tail)))))
					(let-values ([(homes* spill*) 
						(simplify-and-select `(,local* ... ,ulocal* ...) ulocal* rcg*)])
					(if (null? spill*)
						`(locate ([,uvar* ,fvar*] ... ,homes* ...) ,tail)
						`(locals ,(difference local* spill*)
							(ulocals (,ulocal* ...)
								(spills ,spill*
                      (locate ([,uvar* ,fvar*] ...) ;; discard assigned homes*
                        (frame-conflict ,fcg* ,tail)))))))] ;; drop register-conflict form
					[(locate ([,uvar* ,loc*] ...) ,tail)
         `(locate ([,uvar* ,loc*] ...) ,tail)] ;; output same for complete body
         [,x (error who "invalid Body ~s" x)])))
		(lambda (x)
			(match x
				[(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
				`(letrec ([,label* (lambda () ,body*)] ...) ,body)]
				[,x (error who "invalid Program ~s" x)])))
)