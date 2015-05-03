(library (Compiler parse-scheme)
  (export parse-scheme)
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

(define-who parse-scheme

  (define primitives
    '((+ . 2) (- . 2) (* . 2) (<= . 2) (< . 2) (= . 2)
      (>= . 2) (> . 2) (boolean? . 1) (car . 1) (cdr . 1)
      (cons . 2) (eq? . 2) (fixnum? . 1) (make-vector . 1)
      (null? . 1) (pair? . 1) (procedure? . 1) (set-car! . 2)
      (set-cdr! . 2) (vector? . 1) (vector-length . 1)
      (vector-ref . 2) (vector-set! . 3) (void . 0)))

  (define (datum? x)
    (define (constant? x)
      (or (memq x '(#t #f ()))
          (and (and (integer? x) (exact? x))
               (or (fixnum-range? x)
                   (error who "integer ~s is out of fixnum range" x)))))
    (or (constant? x)
        (if (pair? x)
            (and (datum? (car x)) (datum? (cdr x)))
            (and (vector? x) (and (map datum? (vector->list x)))))))

  (define verify-x-list
    (lambda (x* x? what)
      (let loop ([x* x*] [idx* '()])
        (unless (null? x*)
          (let ([x (car x*)] [x* (cdr x*)])
            (unless (x? x)
              (error who "invalid ~s ~s found" what x))
            (let ([idx (extract-suffix x)])
              (when (member idx idx*)
                (error who "non-unique ~s suffix ~s found" what idx))
              (loop x* (cons idx idx*))))))))

  (define (replace-var* var* env)
    (match var*
      [() '()]
      [,x (guard (and #t (assq x env))) (cdr (assq x env))]
      [(,[x] . ,[y]) `(,x . ,y)]
      [,x (error who "var not replaced: ~s" x )]
      ))

  (define (Program x)
    (define all-uvar* '())
    
    (define (Expr env uvar*)
      (lambda (x)
        (match x
          [,k (guard (or (immediate? k) (fixnum? k) (integer? k)))
              `(quote ,k)]
          [,id (guard (symbol? id))
               (if (assq id env)
                   (cdr (assq id env))
                   (error who "unbound variable ~s" id))]
          [(,rator ,rand* ...) (guard (and (assq rator env)))
           (let ([e (Expr env uvar*)])
             (map e `(,rator ,rand* ...)))]
          [(quote ,x) 
           (unless (datum? x) (error who "invalid datum ~s" x))
           `(quote ,x)]
          [(not ,[(Expr env uvar*) -> e]) `(if ,e '#f '#t)]
          [(and) '#t]
          [(and ,[(Expr env uvar*) -> e]) e]
          [(and ,[(Expr env uvar*) -> e] ,e* ...)
           `(if ,e ,((Expr env uvar*) `(and ,e* ...)) '#f)]
          [(or) '#f]
          [(or ,[(Expr env uvar*) -> e]) e]
          [(or ,[(Expr env uvar*) -> e] ,e* ...)
           (let ([tmp (unique-name who)])
             `(let ([,tmp ,e])
                (if ,tmp ,tmp ,((Expr env uvar*) `(or ,e* ...)))))]
          [(if ,[(Expr env uvar*) -> t] ,[(Expr env uvar*) -> c]) (guard (not (assq 'if env)))
           `(if ,t ,c (void))]
          [(if ,[(Expr env uvar*) -> t] ,[(Expr env uvar*) -> c] ,[(Expr env uvar*) -> a])
           `(if ,t ,c ,a)]
          [(begin ,[(Expr env uvar*) -> e*] ... ,[(Expr env uvar*) -> e])
           `(begin ,e* ... ,e)]
          [(lambda (,fml* ...) ,x ,x* ...) (guard (and (not (assq 'lambda env)) (for-all symbol? fml*)))
           (let* ([env+ (map (lambda (fml)
                               (cons fml (unique-name fml))) fml*)]
                  [rep (replace-var* fml* env+)])
             (set! all-uvar* (append rep all-uvar*))
             `(lambda (,rep ...)
                (begin
                  ,@(map (Expr `(,(append env+ env) ...) (append rep uvar*)) (cons x x*)))))]
          [(let ([,new-uvar* ,[(Expr env uvar*) -> x*]] ...) ,e ,e* ...)
           (let* ([env+ (map (lambda (var)
                               (cons var (unique-name var))) new-uvar*)]
                  [rep (replace-var* new-uvar* env+)])
             (set! all-uvar* (append rep all-uvar*))
             `(let ([,rep ,x*] ...)
                ,(let* ([new-env (append env+ env)]
                        [new-uvar* (append rep uvar*)]
                        [expr (Expr new-env new-uvar*)])
                   `(begin ,@(map expr (cons e e*)))
                   )))]
          [(letrec ([,new-uvar* ,e*] ...) ,e ,e+ ...)
           (let* ([env+ (map (lambda (var)
                               (cons var (unique-name var))) new-uvar*)]
                  [rep (replace-var* new-uvar* env+)])
             (set! all-uvar* (append rep all-uvar*))
             (let* ([new-env (append env+ env)]
                    [new-uvar* (append rep uvar*)]
                    [expr (Expr new-env new-uvar*)])
               (if (null? e+)
                   `(letrec ([,rep ,(map expr e*)] ...)
                      (begin ,(expr e)))
                   `(letrec ([,rep ,(map expr e*)] ...)
                      (begin ,@(map expr (cons e e+)))))))]
          [(set! ,uvar ,[(Expr env uvar*) -> x])
           (if (and (assq uvar env) #t)
               `(set! ,(replace-var* uvar env) ,x)
               (error who "unbound uvar ~s" uvar))]
          [(,prim ,[(Expr env uvar*) -> x*] ...)
           (guard (assq prim primitives))
           (unless (= (length x*) (cdr (assq prim primitives)))
             (error who "too many or few arguments ~s for ~s" (length x*) prim))
           `(,prim ,x* ...)]
          [(,rator ,rand* ...)
           (let ([e (Expr env uvar*)])
             (map e `(,rator ,rand* ...)))]
          [,x (error who "invalid Expr ~s" x)])))
    
    (let ([x ((Expr '() '()) x)])
      (verify-x-list all-uvar* uvar? 'uvar)
      x))
  
  (lambda (x) (Program x))
  
  ))