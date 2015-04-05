(library (Compiler normalize-context)
  (export normalize-context)
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
  
  (define-who normalize-context
    
    (define (make-nopless-begin x*)
      (let ([x* (remove '(nop) x*)])
        (if (null? x*)
          '(nop)
          (make-begin x*))))
    
    (define (Value value)
      (define (handle-prim prim vl*)
        (match prim
        [,ef-prim (guard (effect-prim? ef-prim)) (make-nopless-begin `((,prim ,vl* ...) (void)))] ;; nopless-begin
        [,pr-prim (guard (pred-prim?   pr-prim)) `(if (,prim ,vl* ...) '#t '#f)]
        [,vl-prim (guard (value-prim?  vl-prim)) `(,prim ,vl* ...)]
        ))
      (match value
        [(quote ,[Immediate -> im]) `(quote ,im)]
        [(begin ,[Effect -> ef*] ... ,[vl]) (make-begin `(,ef* ... ,vl))]
        [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
        [(let ([,uv* ,[Value -> vl*]] ...) ,[vl]) `(let ([,uv* ,vl*] ...) ,vl)]
        [(,prim ,[vl*] ...) (guard (prim? prim)) (handle-prim prim vl*)]
        [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
        [,lbl (guard (label? lbl)) lbl]
        [,uv (guard (uvar? uv)) uv]
        ))

    (define (Effect effect)
      (define (handle-prim prim vl*)
        (match prim
          [,ef-prim (guard (effect-prim? ef-prim)) `(,prim ,vl* ...)]
          [,else `(nop)]
          ))
      (match effect
        [,lbl (guard (label? lbl))  `(nop)]
        [,uv  (guard (uvar? uv))    `(nop)]
        [(quote ,[Immediate -> im]) `(nop)]
      [(begin ,[Effect -> ef*] ... ,[ef]) (make-nopless-begin `(,ef* ... ,ef))] ;; nopless-begin
      [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
      [(let ([,uv* ,[Value -> vl*]] ...) ,[ef]) `(let ([,uv* ,vl*] ...) ,ef)]
      [(,prim ,[Value -> vl*] ...) (guard (prim? prim)) (handle-prim prim vl*)]
      [(,[Value -> rator] ,[Value -> rand*] ...) `(,rator ,rand* ...)]
      ))

    (define (Pred pred)
      (define (handle-prim prim vl*)
        (match prim
          [,pr-prim (guard (pred-prim?   pr-prim)) `(,prim ,vl* ...)]
        [,ef-prim (guard (effect-prim? ef-prim)) (make-nopless-begin `((,prim ,vl* ...) (true)))] ;; ehhh.
        [,else `(if (eq? (,prim ,vl* ...) '#f) (false) (true))]
        ))
      (match pred
        [void `(nop)]
        [(begin ,[Effect -> ef*] ... ,[pr]) (make-begin `(,ef* ... ,pr))]
        [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
        [(let ([,uv* ,[Value -> vl*]] ...) ,[pr]) `(let ([,uv* ,vl*] ...) ,pr)]
        [(quote ,[Immediate -> im]) (if (eq? im #f) '(false) '(true))]
        [(,prim ,vl* ...) (guard (prim? prim)) (handle-prim prim vl*)]
        [,const (guard (or (uvar? const) (label? const)))
        `(if (eq? ,const '#f) (false) (true))]
        ))


    (define (Immediate immediate)
      (match immediate
        [() '()]
        [#t '#t]
        [#f '#f]
        [,fixnum (guard (fixnum? fixnum)) fixnum]
        ))

    (lambda(x)
      (match x
       [(letrec ([,label (lambda (,uvar* ...) ,[Value -> vl*])] ...) ,[Value -> vl])
       `(letrec ([,label (lambda (,uvar* ...) ,vl*)] ...) ,vl)]))
    )
);end 