(library (Compiler uncover-assigned)
  (export uncover-assigned)
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

(define-who uncover-assigned
  (define remove-common
    (lambda (s)
      (cond
        [(null? s) '()]
        [else (cons (car s) (remove-common (remq (car s) (cdr s))))])))
  (define (Expr ex)
    (match ex
      [(begin ,[ex* a**] ... ,[ex a*])
       (values (make-begin `(,ex* ... ,ex)) 
         (remove-common `(,a** ... ... ,a* ...)))]
      [(if ,[pred pa*] ,[conseq ca*] ,[alter aa*])
       (values `(if ,pred ,conseq ,alter) 
         (remove-common `(,pa* ... ,ca* ... ,aa* ...)))]
      [(let ([,uvar* ,[ex* a**]] ...) ,[ex a*])
       (let ([a* (intersection a* uvar*)]
             [rest* (remove-common `(,a** ... ... ,(difference a* uvar*) ...))])
         (values `(let ([,uvar* ,ex*] ...)
                    (assigned ,a* ,ex)) rest*))]
      [(letrec ([,uvar* ,[ex* a**]] ...) ,[ex a*])
       (let ([t* (remove-common `(,a** ... ... ,a* ...))])
         (let ([a* (intersection t* uvar*)] [rest* (difference t* uvar*)])
           (values `(letrec ([,uvar* ,ex*] ...) (assigned ,a* ,ex)) rest*)))]
      [(lambda (,fml* ...) ,[ex a*])
       (let ([a* (intersection a* fml*)] [rest* (difference a* fml*)])
         (values `(lambda (,fml* ...) (assigned ,a* ,ex)) rest*))]
      [(set! ,uvar ,[ex a*])
       (values `(set! ,uvar ,ex) (remove-common `(,uvar ,a* ...)))]
      [(quote ,imm) (values `(quote ,imm) '())]
      [,uvar (guard (uvar? uvar)) (values uvar `())]
      [(,prim ,[ex* a**] ...) (guard (prim? prim))
       (values `(,prim ,ex* ...) (remove-common `(,a** ... ...)))]
      [(,[rator a*] ,[rand* a**] ...)
       (values `(,rator ,rand* ...) (remove-common `(,a* ... ,a** ... ...)))]
      ))
  (lambda (x)
    (match x
      [,[Expr -> ex a*]ex])))
)