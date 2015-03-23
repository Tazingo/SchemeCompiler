(library (Compiler specify-representation)
	(export specify-representation)
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


(define-who specify-representation
  (lambda (x)
    (define trivial?
      (lambda (x)
        (or (number? x) (memq x (list $false $true $nil $void)))))
    (match x
      ; basic grammar structures
      [(letrec ([,label (lambda (,uvar* ...) ,[body*])] ...) ,[body])
       `(letrec ([,label (lambda (,uvar* ...) ,body*)] ...) ,body)]
      [(if ,[test] ,[conseq] ,[alt])
       `(if ,test ,conseq ,alt)]
      [(begin ,[ef*] ...)
       `(begin ,ef* ...)]
      [(let ([,x* ,[v*]] ...) ,[body])
       `(let ([,x* ,v*] ...) ,body)]

      ; type and equivalence predicates
      [(eq? ,[x] ,[y]) `(= ,x ,y)]
      [(null? ,[e])
       (specify-representation `(eq? ,e '()))]
      [(pair? ,[e])
       `(= (logand ,e ,mask-pair) ,tag-pair)]
      [(boolean? ,[e])
       `(= (logand ,e ,mask-boolean) ,tag-boolean)]
      [(fixnum? ,[e])
       `(= (logand ,e ,mask-fixnum) ,tag-fixnum)]
      [(vector? ,[e])
       `(= (logand ,e ,mask-vector) ,tag-vector)]

      ; numbers
      [(,op ,[m] ,[n]) (guard (binop? op))
       (cond
        [(not (eq? op '*))`(,op ,m ,n)]
        [(number? m) `(* ,(sra m shift-fixnum) ,n)]
        [(number? n) `(* ,(sra n shift-fixnum) ,m)]
        [else `(* (sra ,m ,shift-fixnum) ,n)])]

      ; pairs
      [(cons ,[a] ,[d])
         (let ([avar (unique-name 'car)]
               [dvar (unique-name 'cdr)]
               [ls (unique-name 'ls)])
           `(let ([,avar ,a]
                  [,dvar ,d])
              (let ([,ls (+ (alloc ,size-pair) ,tag-pair)])
                (begin
                  (mset! ,ls ,(- disp-car tag-pair) ,avar)
                  (mset! ,ls ,(- disp-cdr tag-pair) ,dvar)
                  ,ls))))]
      [(set-car! ,[e1] ,[e2])
       (let ([offset-car (- disp-car tag-pair)])
         `(mset! ,e1 ,offset-car ,e2))]
      [(set-cdr! ,[e1] ,[e2])
       (let ([offset-cdr (- disp-cdr tag-pair)])
         `(mset! ,e1 ,offset-cdr ,e2))]
      [(car ,[e])
       (let ([offset-car (- disp-car tag-pair)])
         `(mref ,e ,offset-car))]
      [(cdr ,[e])
       (let ([offset-cdr (- disp-cdr tag-pair)])
         `(mref ,e ,offset-cdr))]

      ; vectors
      [(make-vector ,[k])
       (let ([offset-vector-length (- disp-vector-length tag-vector)]
             [tmp (unique-name 't)])
         (cond
          [(number? k)
           `(let ([,tmp (+ (alloc ,(+ disp-vector-data k)) ,tag-vector)])
              (begin
                (mset! ,tmp ,offset-vector-length ,k)
                ,tmp))]
          [else
           (let ([tmp1 (unique-name 't)]
                 [tmp2 (unique-name 't)])
             `(let ([,tmp1 ,k])
                (let ([,tmp2 (+ (alloc (+ ,disp-vector-data ,tmp1)) ,tag-vector)])
                  (begin
                    (mset! ,tmp2 ,offset-vector-length ,tmp1)
                    ,tmp2))))]))]
      [(vector-set! ,[e1] ,[e2] ,[e3])
       (let ([offset-vector-data (- disp-vector-data tag-vector)])
         (cond
          [(number? e2)
           `(mset! ,e1 ,(+ offset-vector-data e2) ,e3)]
          [else
           `(mset! ,e1 (+ ,offset-vector-data ,e2) ,e3)]))]
      [(vector-ref ,[e1] ,[e2])
       (let ([offset-vector-data (- disp-vector-data tag-vector)])
         (cond
          [(number? e2)
           `(mref ,e1 ,(+ offset-vector-data e2))]
          [else
           `(mref ,e1 (+ ,offset-vector-data ,e2))]))]
      [(vector-length ,[e1])
       (let ([offset-vector-length (- disp-vector-length tag-vector)])
         `(mref ,e1 ,offset-vector-length))]


      ; immediates
      [(quote ,n) (guard (number? n))
       (ash n shift-fixnum)]
      [(quote #f) $false]
      [(quote #t) $true]
      [(quote ()) $nil]
      [(void) $void]

      ; procedure calls goes last because it could match other cases
      [(,[f] ,[x*] ...) `(,f ,x* ...)]
      [,x x])))

);end library