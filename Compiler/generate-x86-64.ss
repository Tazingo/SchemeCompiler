#!chezscheme
(library (Compiler generate-x86-64)
  (export generate-x86-64)
  (import (chezscheme)
	  (Framework helpers)
	  (Framework GenGrammars l01-verify-scheme)
	  (Framework match))

(define-who generate-x86-64
  (define prim->opcode
    (lambda (prim)
      (cdr (assq prim
             '((+ . addq) (- . subq) (* . imulq)
               (logand . andq) (logor . orq) (sra . sarq))))))
  (define Code
    (lambda (ef)
      (match ef
        [,lab (guard (label? lab)) (emit-label lab)]
        [(jump ,rand) (emit-jump 'jmp rand)]
        [(set! ,rand1 ,lab)
         (guard (label? lab))
         (emit 'leaq lab rand1)]
        [(set! ,rand1 (,prim ,rand1 ,rand2))
         (emit (prim->opcode prim) rand2 rand1)]
        [(set! ,rand1 ,rand2) (emit 'movq rand2 rand1)]
        [,ef (error who "invalid Code syntax ~s" ef)])))
  (lambda (x)
    (verify-grammar:l01-verify-scheme x)
    (match x
      [(code ,code* ...) (emit-program (for-each Code code*))]
      [,x (error who "invalid Program syntax ~s" x)])))

)
