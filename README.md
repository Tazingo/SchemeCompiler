# SchemeCompiler
This project is for Compiler course CSCI-P423 @ Spring 2015  

The supported BNF is listed as below.
```
Program   ::= Expr
Expr      ::= Constant
            | Var
            | (quote Datum)
            | (if Expr Expr)
            | (if Expr Expr Expr)
            | (and Expr *)
            | (or Expr *)
            | (begin Expr * Expr)
            | (lambda (Var *) Expr +)
            | (let ([Var Expr] *) Expr +)
            | (letrec ([Var Expr] *) Expr +)
            | (set! Var Expr)
            | (prim Expr *)
            | (Expr Expr *)
Datum     ::= Constant | (Datum *) | #(Datum *)
Constant  ::= fixnum | () | #t | #f
Var       ::= an arbitrary symbol
```
