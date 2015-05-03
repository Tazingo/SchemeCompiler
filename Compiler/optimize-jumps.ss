(library (Compiler optimize-jumps)
  (export optimize-jumps)
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
(define-who optimize-jumps
  (define set-jumps!
    (lambda (j label target)
      (match j
        [() (void)]
        [([,l . ,t] ,j* ...)
         (begin
           (if (eq? label t) (set-cdr! (car j) target))
           (set-jumps! j* label target))])))
  (define listj
    (lambda (bnd* j)
      (match bnd*
        [() j]
        [([,l . (,t)] ,bnd* ...)
         (if (label? t)
             (let ([kv (assq t j)])
               (if kv
                   (if (eq? l (cdr kv))
                       (listj bnd* j)
                       (listj bnd* (cons `(,l . ,(cdr kv)) j)))
                   (begin (set-jumps! j l t)
                     (listj bnd* (cons `(,l . ,t) j)))))
             (listj bnd* j))]
        [([,l . ,t] ,bnd* ...) (listj bnd* j)])))
  (define (Triv j)
    (lambda (t)
      (match t
        [,l (guard (label? l)) 
         (let ([kv (assq l j)])
           (if kv (cdr kv) l))]
        [,x (guard (or (register? x) (disp-opnd? x)
                       (index-opnd? x) (integer? x))) x])))
  (define (Effect j)
    (lambda (ef)
      (match ef
        [(set! ,loc (,binop ,[(Triv j) -> x] ,[(Triv j) -> y]))
         `(set! ,loc (,binop ,x ,y))]
        [(set! ,loc ,[(Triv j) -> t])
         `(set! ,loc ,t)])))
  (define (Tail j)
    (lambda (tail)
      (match tail
        [(begin ,[(Effect j) -> ef*] ... ,[tail])
         (make-begin `(,ef* ... ,tail))]
        [(if (,relop ,[(Triv j) -> x] ,[(Triv j) -> y])
             ,[conseq] ,[alter])
         `(if (,relop ,x ,y) ,conseq ,alter)]
        [(,[(Triv j) -> t]) `(,t)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,tail*)] ...) ,tail)
       (let ([bnd* `([,label* . ,tail*] ...)])
         (let ([j (listj bnd* '())])
           (let ([bnd* (filter (lambda (bnd) 
                                 (not (assq (car bnd) j))) bnd*)])
             `(letrec ([,(map car bnd*) 
                         (lambda () ,(map (lambda (bnd)
                                            ((Tail j) (cdr bnd)))
                                          bnd*))] ...)
                ,((Tail j) tail)))))]))) 
);end library