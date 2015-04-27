(library (Compiler purify-letrec)
  (export purify-letrec)
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

(define-who purify-letrec
  (define (Expr ex)
    (match ex
      [(begin ,[ex*] ... ,[ex])
       (make-begin `(,ex* ... ,ex))]
      [(if ,[pred] ,[conseq] ,[alter])
       `(if ,pred ,conseq ,alter)]
      [(quote ,imm) `(quote ,imm)]
      [,uvar (guard (uvar? uvar)) uvar]
      [(lambda (,fml* ...) (assigned (,a* ...) ,[ex]))
       `(lambda (,fml* ...) (assigned (,a* ...) ,ex))]
      [(let ([,uvar* ,[ex*]] ...) (assigned (,a* ...) ,[ex]))
       `(let ([,uvar* ,ex*] ...) (assigned (,a* ...) ,ex))]
      [(letrec ([,uvar* (lambda (,fml** ...) 
                           (assigned (,a** ...) ,[ex*]))] ...)
         (assigned () ,[ex]))
       `(letrec ([,uvar* (lambda (,fml** ...) 
                           (assigned (,a** ...) ,ex*))] ...) ,ex)]
      [(letrec ([,uvar* ,[ex*]] ...) (assigned (,a* ...) ,[ex]))
       (let ([t* (map (lambda (x) (unique-name 't)) uvar*)])
         `(let ([,uvar* (void)] ...)
            (assigned (,uvar* ...)
              ,(make-begin
                 `((let ([,t* ,ex*] ...)
                     (assigned ()
                       ,(make-begin `((set! ,uvar* ,t*) ...))))
                   ,ex)))))]
      [(set! ,uvar ,[ex]) `(set! ,uvar ,ex)]
      [(,prim ,ex* ...) (guard (prim? prim))
       `(,prim ,(map Expr ex*) ...)]
      [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]))
  (lambda (x)
    (match x
      [,[Expr -> ex] ex])))
)