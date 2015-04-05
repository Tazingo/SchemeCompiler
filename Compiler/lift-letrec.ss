(library (Compiler lift-letrec)
  (export lift-letrec)
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

(define-who lift-letrec
  (define primitives '(+ - * car cdr cons make-vector vector-length vector-ref void
                  < <= = >= > boolean? eq? fixnum? null? pair? vector?
                  set-car! set-cdr! vector-set!))
  (define (Expr expr)
    (match expr
      [(begin ,[e* b**] ... ,[body bb*])
       (values (make-begin `(,e* ... ,body))
         `(,b** ... ... ,bb* ...))]
      [(if ,[t tbnd*] ,[c cbnd*] ,[a abnd*])
       (values `(if ,t ,c ,a) `(,tbnd* ... ,cbnd* ... ,abnd* ...))]
      [(quote ,imm) (values `(quote ,imm) '())]
      [(let ([,uvar* ,[e* b**]] ...) ,[body bb*])
       (values `(let ([,uvar* ,e*] ...) ,body) 
         `(,b** ... ... ,bb* ...))]
      [(letrec ([,label* (lambda (,fml** ...) ,[e* b**])] ...) ,[body bb*])
       (values body 
         `([,label* (lambda (,fml** ...) ,e*)] ... ,b** ... ... ,bb* ...))]
      [,label (guard (label? label)) (values label '())]
      [,uvar (guard (uvar? uvar)) (values uvar '())]
      [(,prim ,[e* b**] ...) (guard (memq prim primitives))
       (values `(,prim ,e* ...) `(,b** ... ...))]
      [(,[rator ratorb*] ,[rand* randb**] ...)
       (values `(,rator ,rand* ...) `(,ratorb* ... ,randb** ... ...))]
      [,x (error who "invalid Expr ~s" x)]))
  (lambda (x)
    (match x
      [,[Expr -> e b*] `(letrec (,b* ...) ,e)]))) 
)