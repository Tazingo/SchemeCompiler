(library (Compiler optimize-direct-call)
  (export optimize-direct-call)
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
  
  (define-who optimize-direct-call
    (define (Lambda lamb)
      (match lamb
        [(lambda (,fml* ...) ,[Expr -> ex])
        `(lambda (,fml* ...) ,ex)]))
    (define (Expr ex)
      (match ex
        [(begin ,[ex*] ... ,[ex])
        (make-begin `(,ex* ... ,ex))]
        [(if ,[pred] ,[conseq] ,[alter])
        `(if ,pred ,conseq ,alter)]
        [(quote ,imm) `(quote ,imm)]
        [(let ([,uvar* ,[ex*]] ...) ,[ex])
        `(let ([,uvar* ,ex*] ...) ,ex)]
        [(letrec ([,uvar* ,[Lambda -> lamb*]] ...) ,[ex])
        `(letrec ([,uvar* ,lamb*] ...) ,ex)]
        [(,prim ,ex* ...) (guard (prim? prim))  
        `(,prim ,(map Expr ex*) ...)]
        [,uvar (guard (uvar? uvar)) uvar]
        [,lamb (guard (lambda? lamb)) (Lambda lamb)]
        
        [((lambda (,fml* ...) ,[ex]) ,[ex*] ...)
        `(let ([,fml* ,ex*] ...) ,ex)]
        [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
        ))
    (lambda (x)
      (match x
        [,[Expr -> ex] ex]))
    )
)