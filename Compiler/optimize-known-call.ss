(library (Compiler optimize-known-call)
  (export optimize-known-call)
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
  
  (define-who optimize-known-call
    (define (Expr cbnd*)
      (lambda (ex)
        (match ex
          [(begin ,[ex*] ... ,[ex])
          (make-begin `(,ex* ... ,ex))]
          [(if ,[pred] ,[conseq] ,[alter])
          `(if ,pred ,conseq ,alter)]
          [(quote ,imm) `(quote ,imm)]
          [(let ([,uvar* ,[ex*]] ...) ,[ex])
          `(let ([,uvar* ,ex*] ...) ,ex)]
          [(letrec ([,label* (lambda (,fml** ...) (bind-free (,f** ...) ,ex*))] ...)
           (closures ([,cp* ,lb* ,uvar** ...] ...) ,ex))
          (let ([cbnd* `((,cp* . ,lb*) ... ,cbnd* ...)])
           (let ([ex* (map (Expr cbnd*)  ex*)]
             [ex ((Expr cbnd*) ex)])
           `(letrec ([,label* (lambda (,fml** ...) (bind-free (,f** ...) ,ex*))] ...)
            (closures ([,cp* ,lb* ,uvar** ...] ...) ,ex))))]
          [(,prim ,ex* ...) (guard (prim? prim)) 
          `(,prim ,(map (Expr cbnd*) ex*) ...)]
          [,uvar (guard (uvar? uvar)) uvar]
          [,label (guard (label? label)) label]
          [(,[rator] ,[rand*] ...)
          (if (uvar? rator)
           (let ([cbnd (assq rator cbnd*)])
             `(,(if cbnd (cdr cbnd) rator) ,rand* ...))
           `(,rator ,rand* ...))]
          [,x (error who "invalid Expr ~s" x)])))
    (lambda (x)
      (match x
        [,[(Expr '()) -> ex] ex]))

    )
)