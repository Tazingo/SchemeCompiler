(library (Compiler remove-anonymous-lambda)
  (export remove-anonymous-lambda)
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
  
  (define-who remove-anonymous-lambda
      (define (Lambda rhs)
        (lambda (lamb)
          (match lamb
            [(lambda (,fml* ...) ,[(Expr #f) -> ex])
            (if rhs
               `(lambda (,fml* ...) ,ex)
               (let ([anon (unique-name 'anon)])
                 `(letrec ([,anon (lambda (,fml* ...) ,ex)])
                  ,anon)))]
            )))
      (define (Expr rhs)
        (lambda (ex)
          (match ex
            [(begin ,[(Expr #f) -> ex*] ... ,[(Expr #f) -> ex])
            (make-begin `(,ex* ... ,ex))]
            [(if ,[(Expr #f) -> pred] ,[(Expr #f) -> conseq] ,[(Expr #f) -> alter])
            `(if ,pred ,conseq ,alter)]
            [(quote ,imm) `(quote ,imm)]
            [(let ([,uvar* ,[(Expr #t) -> ex*]] ...) ,[(Expr #f) -> ex])
            `(let ([,uvar* ,ex*] ...) ,ex)]
            [(letrec ([,uvar* ,[(Lambda #t) -> lamb*]] ...) ,[(Expr #f) -> ex])
            `(letrec ([,uvar* ,lamb*] ...) ,ex)]
            [(,prim ,ex* ...) (guard (prim? prim))
            `(,prim ,(map (Expr #f) ex*) ...)]
            [,lamb (guard (lambda? lamb)) ((Lambda rhs) lamb)]
            [,uvar (guard (uvar? uvar)) uvar]
            [(,[(Expr #f) -> rator] ,[(Expr #f) -> rand*] ...)
            `(,rator ,rand* ...)]
            )))
      (lambda (x)
        (match x
          [,[(Expr #f) -> ex] ex])))

)