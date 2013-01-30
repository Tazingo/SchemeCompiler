(library (Compiler flatten-program)
  (export flatten-program)
  (import (chezscheme)
          (Framework helpers)
          (Framework match))

;; This pass flattens programs by moving letrec-bound code blocks
;  into a single, flat code listing.
(define-who flatten-program
  (define Effect
    (lambda (ef)
      (match ef
        [(set! ,var ,rhs) `((set! ,var ,rhs))]
        [,ef (error who "invalid syntax for Effect ~s" ef)])))
  (define Tail
    (lambda (tail)
      (match tail
        [(,t) `((jump ,t))]
        [(begin ,[Effect -> code**] ... ,[code*])
         `(,code** ... ... ,code* ...)]
        [,tail (error who "invalid syntax for Tail ~s" tail)])))
  (lambda (program)
    (match program
       [(letrec ([,label* (lambda () ,[Tail -> code**])] ...) ,[Tail -> code*])
	(let ([code** (map cons label* code**)])
	  `(code ,code* ... ,code** ... ...))]
       [,program (error who "invalid syntax for Program: ~s" program)])))

)
