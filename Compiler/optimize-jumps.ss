(library (Compiler optimize-jumps)
  (export optimize-jumps)
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
  (define-who (optimize-jumps program)
    
    (define bindings '())
    (define (associate-jumps lbl jmp)
      (let loop ([end jmp][nxt (cons '0 jmp)])
        (if nxt (loop (cdr nxt) (assoc (cdr nxt) bindings))
          (set! bindings (cons (cons lbl end) bindings)))))
    (define (resolve-jump lbl)
      (let loop ([last lbl][site (assq lbl bindings)])
        (if site (loop (cdr site) (assoc (cdr site) bindings))
          last)))
    (define (Effect effect)
      (match effect
        [(nop) '(nop)]
        [(begin ,[Effect -> ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
        [(if ,[Pred -> pr] (,cjmp) (,ajmp)) `(if ,pr (,(resolve-jump cjmp)) (,(resolve-jump ajmp)))]
        [(set! ,uv ,lbl) (guard (label? lbl)) `(set! ,uv ,(resolve-jump lbl))]
        [(set! ,uv . ,x) `(set! ,uv . ,x)]
        ))
    (define (Pred pred)
      (match pred
        [(true) '(true)]
        [(false) '(false)]
        [(begin ,[Effect -> ef*] ... ,pjmp) (make-begin `(,ef* ... ,(resolve-jump pjmp)))]
        [(if ,[Pred -> pr] (,cjmp) (,ajmp)) `(if ,pr (,(resolve-jump cjmp)) (,(resolve-jump ajmp)))]
        [(,relop ,tr ,tr^) (guard (relop? relop)) `(,relop ,tr ,tr^)]
        ))
    (define (Tail tail)
      (match tail
        [(begin ,[Effect -> ef*] ... ,[tl]) (make-begin `(,ef* ... ,tl))]
        [(if ,[Pred -> pr] (,cjmp) (,ajmp)) `(if ,pr (,(resolve-jump cjmp)) (,(resolve-jump ajmp)))]
        [(,jmp) (guard (triv? jmp)) `(,(resolve-jump jmp))]
        [,else else]
        ))
    (define (Bind bind)
      (match bind
        [[,lbl (lambda () (,jmp))] (guard (label? jmp))
        (associate-jumps lbl jmp) bind]
        [[,lbl (lambda () ,tl)]
        `[,lbl (lambda () ,(Tail tl))]]
        ))
    (define (Program p)
      (match p
        [(letrec (,[Bind -> bn*] ...) ,tl)
        `(letrec (,bn* ...) ,(Tail tl))]
        ))

    (Program program)

    )
);end library