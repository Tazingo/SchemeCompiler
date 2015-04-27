(library (Compiler convert-assignments)
  (export convert-assignments)
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

(define-who convert-assignments
  (define (Lambda a*)
    (lambda (lamb)
      (match lamb
        [(lambda (,fml* ...) (assigned (,as* ...) ,ex))
         (if (null? as*)
             `(lambda (,fml* ...) ,((Expr a*) ex))
             (let ([tb* (map (lambda (as) `(,as . ,(unique-name 't))) as*)])
               (let ([fml* (map (lambda (fml)
                                  (let ([tb (assq fml tb*)])
                                    (if tb (cdr tb) fml))) fml*)])
                 `(lambda (,fml* ...)
                    (let ([,(map car tb*) (cons ,(map cdr tb*) (void))] ...)
                      ,((Expr `(,a* ... ,as* ...)) ex))))))])))
  (define (Expr a*)
    (lambda (ex)
      (match ex
        [(begin ,[ef*] ... ,[ef])
         (make-begin `(,ef* ... ,ef))]
        [(if ,[pred] ,[conseq] ,[alter])
         `(if ,pred ,conseq ,alter)]
        [(quote ,imm) `(quote ,imm)]
        [(letrec ([,uvar* ,[(Lambda a*) -> lamb*]] ...) ,[ex])
         `(letrec ([,uvar* ,lamb*] ...) ,ex)]
        [(let ([,uvar* ,[ex*]] ...) (assigned (,as* ...) ,ex))
         (if (null? as*)
             `(let ([,uvar* ,ex*] ...) ,((Expr a*) ex))
             (let ([tb* (map (lambda (as) `(,as . ,(unique-name 't))) as*)])
               (let ([uvar* (map (lambda (uvar)
                                   (let ([tb (assq uvar tb*)])
                                     (if tb (cdr tb) uvar))) uvar*)])
                 `(let ([,uvar* ,ex*] ...)
                    (let ([,(map car tb*) (cons ,(map cdr tb*) (void))] ...)
                      ,((Expr `(,a* ... ,as* ...)) ex))))))]
        [(set! ,uvar ,[ex]) `(set-car! ,uvar ,ex)]
        [,uvar (guard (uvar? uvar))
         (if (memq uvar a*) `(car ,uvar) uvar)]
        [,lamb (guard (lambda? lamb)) 
          ((Lambda a*) lamb)]
        [(,prim ,ex* ...) (guard (prim? prim))
         `(,prim ,(map (Expr a*) ex*) ...)]
        [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
        
        )))
  (lambda (x)
    (match x
      [,[(Expr '()) -> ex] ex]))) 
)