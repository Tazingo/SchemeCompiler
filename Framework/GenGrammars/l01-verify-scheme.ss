;; Automatically generated file -- DO NOT MODIFY
(library (Framework GenGrammars l01-verify-scheme)
  (export verify-grammar:l01-verify-scheme)
  (import (chezscheme) (Framework match) (Framework prims))
  (define (any . nested-bool-ls)
    (letrec ([helper (lambda (x)
                       (cond
                         [(not x) #f]
                         [(null? x) #f]
                         [(pair? x) (or (helper (car x)) (helper (cdr x)))]
                         [else x]))])
      (helper nested-bool-ls)))
  (define verify-grammar:l01-verify-scheme
    (lambda (x)
      (define Prog
        (lambda (x)
          (match x
            [(begin ,(Statement -> x1) ... ,(Statement -> x2))
             (any x2 x1)]
            [,e (invalid-expr 'Prog e)])))
      (define Statement
        (lambda (x)
          (match x
            [(set! . ,bod)
             (and (match (cons 'set! bod)
                    [(set! ,(Var -> x1) ,(Integer -> x2)) (any x2 x1)]
                    [,e (invalid-expr 'set! e)])
                  (match (cons 'set! bod)
                    [(set! ,(Var -> x1) ,(Var -> x2)) (any x2 x1)]
                    [,e (invalid-expr 'set! e)])
                  (match (cons 'set! bod)
                    [(set! ,(Var -> x1)
                       (,(Binop -> x2) ,(Var -> x3) ,(Integer -> x4)))
                     (any x4 x3 x2 x1)]
                    [,e (invalid-expr 'set! e)])
                  (match (cons 'set! bod)
                    [(set! ,(Var -> x1)
                       (,(Binop -> x2) ,(Var -> x3) ,(Var -> x4)))
                     (any x4 x3 x2 x1)]
                    [,e (invalid-expr 'set! e)]))]
            [,e (invalid-expr 'Statement e)])))
      (define Var
        (lambda (x)
          (match x
            [,e (guard (not [Reg e])) #f]
            [,e (invalid-expr 'Var e)])))
      (let ([res (Prog x)])
        (if res
            (errorf 'verify-grammar:l01-verify-scheme "~a" res)
            x)))))
