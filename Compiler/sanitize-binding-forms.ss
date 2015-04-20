(library (Compiler sanitize-binding-forms)
  (export sanitize-binding-forms)
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
  
  (define-who sanitize-binding-forms
      (define (Lambda lamb)
        (match lamb
          [(lambda (,fml* ...) ,[Expr -> ex])
          `(lambda (,fml* ...) ,ex)]
          ))
      (define (Expr ex)
        (match ex
          [(begin ,[ex*] ... ,[ex])
          (make-begin `(,ex* ... ,ex))]
          [(if ,[pred] ,[conseq] ,[alter])
          `(if ,pred ,conseq ,alter)]
          [(quote ,imm) `(quote ,imm)]
          [(let (,bnd* ...) ,[ex])
          (let ([lambnd* (filter (lambda (bnd) (lambda? (cadr bnd))) bnd*)])
           (let ([exbnd* (difference bnd* lambnd*)])
             (cond
               [(null? bnd*) ex]
               [(null? lambnd*) 
               `(let ([,(map car exbnd*) ,(map Expr (map cadr exbnd*))] ...) ,ex)]
               [(null? exbnd*) 
               `(letrec ([,(map car lambnd*) ,(map Lambda (map cadr lambnd*))] ...) ,ex)]
               [else
               `(letrec ([,(map car lambnd*) ,(map Lambda (map cadr lambnd*))] ...)
                   (let ([,(map car exbnd*) ,(map Expr (map cadr exbnd*))] ...)
                    ,ex))])))]
          [(letrec ([,uvar* ,[Lambda -> lamb*]] ...) ,[ex])
          (if (null? uvar*) ex `(letrec ([,uvar* ,lamb*] ...) ,ex))]
          [(,prim ,ex* ...) (guard (prim? prim))
          `(,prim ,(map Expr ex*) ...)]
          [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
          [,uvar (guard (uvar? uvar)) uvar]
          ))
    (lambda (x)
        (match x
          [,[Expr -> ex] ex])))

)