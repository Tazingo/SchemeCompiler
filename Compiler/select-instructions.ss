(library (Compiler select-instructions)
  (export select-instructions)
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

  (define-who select-instructions
  (define com* '(+ * logand logor)) ;; specifies the commutative operators
  (define reverse* '((< . >) (<= . >=) (> . <) (>= . <=) (= . =))) ;; specifies reverse of relops
  (define uvar-or-reg?
    (lambda (triv)
      (or (register? triv) (uvar? triv))))
  (define var?
    (lambda (triv)
      (or (uvar-or-reg? triv) (frame-var? triv))))
  (define strictly-int64?
    (lambda (x)
      (and (not (int32? x)) (int64? x))))
  (define Body
    (lambda (body)
      (define newulocal* '())
      (define newu
        (lambda ()
          (set! newulocal* (cons (unique-name 't) newulocal*))
          (car newulocal*)))
      (define (Pred ulocal*)
        (lambda (pred)
          (match pred
            [(true) '(true)]
            [(false) '(false)]
            [(if ,[pred] ,[conseq] ,[alter])
            `(if ,pred ,conseq ,alter)]
            [(begin ,[(Effect ulocal*) -> ef*] ... ,[pred])
            (make-begin `(,ef* ... ,pred))]
            [(,relop ,x ,y) (Relop `(,relop ,x ,y))]
            [,x (error who "invalid Pred ~s" x)])))
      (define (Effect ulocal*)
        (lambda (ef)
          (match ef
            [(nop) '(nop)]
            [(if ,[(Pred ulocal*) -> pred] ,[conseq] ,[alter])
            `(if ,pred ,conseq ,alter)]
            [(begin ,[ef*] ...) (make-begin ef*)]
            [(return-point ,label ,[(Tail ulocal*) -> tail]) 
            `(return-point ,label ,tail)]
            [(mset! ,b ,o ,v) (MemOp `(mset! ,b ,o ,v) ulocal*)]
            [(set! ,x (mref ,b ,o)) (MemOp `(set! ,x (mref ,b ,o)) ulocal*)] 
            [(set! ,x (,binop ,y ,z)) (Binop `(set! ,x (,binop ,y ,z)))]
            [(set! ,x ,y) (Move `(set! ,x ,y))])))
      (define (Tail ulocal*)
        (lambda (tail)
          (match tail
            [(begin ,[(Effect ulocal*) -> ef*] ... ,[(Tail ulocal*) -> tail])
            (make-begin `(,ef* ... ,tail))]
            [(if ,[(Pred ulocal*) -> pred] ,[conseq] ,[alter])
            `(if ,pred ,conseq ,alter)]
            [(,triv ,loc* ...) `(,triv ,loc* ...)]
            [,x (error who "invalid Tail ~s" x)])))
      (define MemOp
        (lambda (memop ulocal*)
          (match memop
            [(mset! ,b ,o ,v)
            (let ([ub (if (not (memq b ulocal*)) (newu) b)]
             [uo (if (not (memq o ulocal*)) (newu) o)]
             [uv (if (not (memq v ulocal*)) (newu) v)])
            (make-begin
             `((set! ,ub ,b)
               (set! ,uo ,o)
               (set! ,uv ,v)
               (mset! ,ub ,uo ,uv))))]
            [(set! ,x (mref ,b ,o))
            (let ([ub (if (not (memq b ulocal*)) (newu) b)]
             [uo (if (not (memq o ulocal*)) (newu) o)]
             [ux (if (not (memq x ulocal*)) (newu) x)])
            (make-begin
             `((set! ,ub ,b)
               (set! ,uo ,o)
               (set! ,ux (mref ,ub ,uo))
               (set! ,x ,ux))))])))
      (define Move
        (lambda (ef)
          (match ef
            [(set! ,x ,y)
            (if (and (frame-var? x) (or (frame-var? y) (strictly-int64? y) (label? y)))
             (let ([u (newu)])
               (make-begin
                `((set! ,u ,y)
                  (set! ,x ,u))))
             `(set! ,x ,y))])))
      (define Binop
        (lambda (ef)
          (match ef
            [(set! ,x (,op ,y ,z))
            (cond
              [(eq? x y) (Binop2 `(set! ,x (,op ,x ,z)))]
              [(and (eq? x z) (memq op com*)) (Binop2 `(set! ,x (,op ,x ,y)))]
              [else (let ([u (newu)])
                (make-begin
                 `((set! ,u ,y)
                   ,(Binop2 `(set! ,u (,op ,u ,z)))
                   (set! ,x ,u))))])])))
      (define Binop2
        (lambda (ef)
          (match ef
            [(set! ,var (,op ,var ,triv))
            (cond
              [(and (eq? '* op)
                (frame-var? var))
              (let ([u (newu)])
               (make-begin
                `((set! ,u ,var)
                  ,(Binop2 `(set! ,u (* ,u ,triv)))
                  (set! ,var ,u))))]
              [(or (and (uvar-or-reg? var)
                (or (var? triv) (int32? triv)))
              (and (frame-var? var)
                (or (uvar-or-reg? triv) (int32? triv))))
              `(set! ,var (,op ,var ,triv))]
              [(or (and (frame-var? var)
                (or (frame-var? triv) (strictly-int64? triv) (label? triv)))
              (and (uvar-or-reg? var)
                (or (strictly-int64? triv) (label? triv))))
              (let ([u (newu)])
               (make-begin
                `((set! ,u ,triv)
                  (set! ,var (,op ,var ,u)))))])])))
      (define Relop
        (lambda (pred)
          (match pred
            [(,op ,triv1 ,triv2)
            (cond
              [(var? triv1) (Relop2 `(,op ,triv1 ,triv2))]
              [(var? triv2) (Relop2 `(,(cdr (assq op reverse*)) ,triv2 ,triv1))]
              [else (let ([u (newu)])
                (make-begin
                 `((set! ,u ,triv1)
                   ,(Relop2 `(,op ,u ,triv2)))))])])))
      (define Relop2
        (lambda (pred)
          (match pred
            [(,op ,var ,triv)
            (cond
              [(or (and (uvar-or-reg? var)
                (or (var? triv) (int32? triv)))
              (and (frame-var? var)
                (or (uvar-or-reg? triv) (int32? triv))))
              `(,op ,var ,triv)]
              [(or (and (uvar-or-reg? var)
                (or (strictly-int64? triv) (label? triv)))
              (and (frame-var? var)
                (or (frame-var? triv) (strictly-int64? triv) (label? triv))))
              (let ([u (newu)])
               (make-begin
                `((set! ,u ,triv)
                  (,op ,var ,u))))])])))

      (match body
        [(locals (,local* ...)
         (ulocals (,ulocal* ...)
           (locate ([,uvar* ,fvar*] ...)
             (frame-conflict ,fcg* ,tail))))
        (let ([tail ((Tail ulocal*) tail)])
         `(locals (,local* ...)
          (ulocals (,ulocal* ... ,newulocal* ...)
            (locate ([,uvar* ,fvar*] ...)
              (frame-conflict ,fcg* ,tail)))))] 
        [(locate ([,uvar* ,loc*] ...) ,tail)
        `(locate ([,uvar* ,loc*] ...) ,tail)]
        [,x (error who "invalid Body ~s" x)])))
(lambda (x)
  (match x
    [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
    `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
    [,x (error who "invalid Program ~s" x)])))
);end library