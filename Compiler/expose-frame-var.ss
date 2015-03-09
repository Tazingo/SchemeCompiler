(library (Compiler expose-frame-var)
  (export expose-frame-var)
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load provided compiler framework:
    (Framework driver)
    (Framework wrappers)
    (Framework match)
    (Framework helpers)
    (Compiler helper))

  (define-who (expose-frame-var p)
    (define fp-offset 0)
    (define (Triv t)
      (match t
        [,t^ (guard (frame-var? t^))
        (make-disp-opnd frame-pointer-register
          (- (ash (frame-var->index t^) word-shift) fp-offset))]
        [,t^ (guard (triv? t^)) t^]
        ))
    (define (Effect e)
      (match e
        [(nop) '(nop)]
        [(begin ,[ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
        [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
        [(return-point ,label ,[Tail -> tl]) `(return-point ,label ,tl)]
        [(set! ,fp (- ,fp ,nb)) (guard (eq? fp frame-pointer-register))
        (set! fp-offset (+ fp-offset nb))
        `(set! ,fp (- ,fp ,nb))]
        [(set! ,fp (+ ,fp ,nb)) (guard (eq? fp frame-pointer-register))
        (set! fp-offset (- fp-offset nb))
        `(set! ,fp (+ ,fp ,nb))]
        [(set! ,[Triv -> uv] (,binop ,[Triv -> tr^] ,[Triv -> tr&]))
        (guard (binop? binop)) `(set! ,uv (,binop ,tr^ ,tr&))]
        [(set! ,[Triv -> uv] ,[Triv -> tr]) `(set! ,uv ,tr)]
        ))
    (define (Pred p)
      (match p
        [(true) '(true)]
        [(false) '(false)]
        [(begin ,[Effect -> ef*] ... ,[pr])
        (make-begin `(,ef* ... ,pr))]
        [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
        [(,relop ,[Triv -> tr] ,[Triv -> tr^]) (guard (relop? relop)) `(,relop ,tr ,tr^)]
        ))
    (define (Tail t)
      (match t
        [(,[Triv -> tr]) `(,tr)]
        [(begin ,[Effect -> ef*] ... ,[tl]) (make-begin `(,ef* ... ,tl))]
        [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
        [,tr (guard (triv? tr)) (Triv tr)]
        ))
    (match p
        [(letrec ([,label (lambda (,uvar* ...) ,[Tail -> tl*])] ...) ,[Tail -> tl])
        `(letrec ([,label (lambda (,uvar* ...) ,tl*)] ...) ,tl)]
        )

    )
  );end library