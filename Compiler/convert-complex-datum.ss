(library (Compiler convert-complex-datum)
  (export convert-complex-datum)
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

(define-who convert-complex-datum
  
  (lambda (x)
    (define bnd* '())
    (define (add-bnd ex)
      (let ([t (unique-name 't)])
        (set! bnd* (cons `(,t ,ex) bnd*))
        t))
    (define (handle-datum d)
      (cond
        [(pair? d) `(cons ,(handle-datum (car d)) ,(handle-datum (cdr d)))]
        [(vector? d)
         (let ([tmp (unique-name 'tmp)] [n (sub1 (vector-length d))])
           `(let ([,tmp (make-vector (quote ,(vector-length d)))])
              ,(make-begin
                (let f ([idx 0])
                  (cons `(vector-set! ,tmp (quote ,idx) ,(handle-datum (vector-ref d idx)))
                        (if (< idx n) (f (add1 idx)) `(,tmp)))))))]
        [(immediate? d) `(quote ,d)]))
    (define (Expr ex)
      (match ex
        [(quote ,datum)
         (if (immediate? datum)
             `(quote ,datum)
             (add-bnd (handle-datum datum)))]
        [(begin ,[ex*] ... ,[ex])
         (make-begin `(,ex* ... ,ex))]
        [(if ,[pred] ,[conseq] ,[alter])
         `(if ,pred ,conseq ,alter)]
        [(lambda (,fml* ...) ,[ex])
         `(lambda (,fml* ...) ,ex)]
        [(let ([,uvar* ,[ex*]] ...) ,[ex])
         `(let ([,uvar* ,ex*] ...) ,ex)]
        [(letrec ([,uvar* ,[ex*]] ...) ,[ex])
         `(letrec ([,uvar* ,ex*] ...) ,ex)]
        [(set! ,uvar ,[ex]) `(set! ,uvar ,ex)]
        [,uvar (guard (uvar? uvar)) uvar]
        [(,prim ,ex* ...) (guard (prim? prim))
         `(,prim ,(map Expr ex*) ...)]
        [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
      ))
    (match x
      [,[Expr -> ex] (if (null? bnd*) ex `(let ,bnd* ,ex))])))
)