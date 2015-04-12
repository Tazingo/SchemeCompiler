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
    
    (define (Effect ef)
      (match ef
        [(begin ,[ef*] ... ,[ef])
        (make-nopless-begin `(,ef* ... ,ef))]
        [(if ,[Pred -> pred] ,[conseq] ,[alter])
        `(if ,pred ,conseq ,alter)]
        [(,prim ,rand* ...) (guard (prim? prim)) 
        (cond
         [(value-prim? prim) 
         (make-nopless-begin (map Effect rand*))] 
         [(pred-prim? prim) 
         (make-nopless-begin (map Effect rand*))]
         [(effect-prim? prim) 
         `(,prim ,(map Value rand*) ...)])]
        [(quote ,imm) '(nop)]
        [(let ([,uvar* ,[Value -> val*]] ...) ,[ef])
        `(let ([,uvar* ,val*] ...) ,ef)]
        [(,[Value -> rator] ,[Value -> rand*] ...)
        `(,rator ,rand* ...)]
        [,x (guard (or (label? x) (uvar? x))) '(nop)]
        ))
    (define (Pred pred)
      (match pred
        [(begin ,[Effect -> ef*] ... ,[pred])
        (make-nopless-begin `(,ef* ... ,pred))]
        [(if ,[pred] ,[conseq] ,[alter])
        `(if ,pred ,conseq ,alter)]
        [(,prim ,[Value -> val*] ...) (guard (prim? prim)) 
        (cond
         [(value-prim? prim) 
         `(if (eq? (,prim ,val* ...) (quote #f)) (false) (true))] 
         [(pred-prim? prim) 
         `(,prim ,val* ...)]
         [(effect-prim? prim) 
         (make-nopless-begin `((,prim ,val* ...) (true)))])]
        [(quote ,imm) (if (eq? imm #f) '(false) '(true))]
        [(let ([,uvar* ,[Value -> val*]] ...) ,[pred])
        `(let ([,uvar* ,val*] ...) ,pred)]
        [(,[Value -> rator] ,[Value -> rand*] ...)
        `(if (eq? (,rator ,rand* ...) (quote #f)) (false) (true))]
        [,label (guard (label? label)) '(true)]      
        [,uvar (guard (uvar? uvar))
        `(if (eq? ,uvar (quote #f)) (false) (true))]
        ))
    (define (Value val)
      (match val
        [(begin ,[Effect -> ef*] ... ,[val])
        (make-nopless-begin `(,ef* ... ,val))]
        [(if ,[Pred -> pred] ,[conseq] ,[alter])
        `(if ,pred ,conseq ,alter)]
        [(,prim ,[val*] ...) (guard (prim? prim)) 
        (cond
         [(value-prim? prim) `(,prim ,val* ...)]
         [(pred-prim? prim) 
         `(if (,prim ,val* ...) (quote #t) (quote #f))]
         [(effect-prim? prim) 
         (make-nopless-begin `((,prim ,val* ...) (void)))])]
        [(quote ,imm) `(quote ,imm)]
        [(let ([,uvar* ,[val*]] ...) ,[val])
        `(let ([,uvar* ,val*] ...) ,val)]
        [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
        [,label (guard (label? label)) label]
        [,uvar (guard (uvar? uvar)) uvar]
        ))
    (lambda (x)
      (match x
        [(letrec ([,label* (lambda (,fml** ...) ,[Value -> val*])] ...)
         ,[Value -> val])
        `(letrec ([,label* (lambda (,fml** ...) ,val*)] ...) ,val)]
        )))
);end 