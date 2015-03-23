(library (Compiler select-instructions)
  (export select-instructions)
  (import (chezscheme) (Framework match) (Framework helpers))

#|
Tables for handling x86_64 constraints in select-instructions

MOVE[(set! Var Triv)]

     ur fvar   int32   int64/label  (ur = uvar or register)
       ------------------------------------
  ur   |   ok ok  ok   ok
  fvar |   ok  X  ok   X

  X: (begin (set! u Triv) (set! Var u))

BINOP1[(set! Var (binop Triv1 Triv2))]

  Triv1 = Var
    BINOP2[(set! Var (binop Var Triv2))]
  Triv1 != Var, Triv2 = Var, binop is commutative (+, *, logor, logand)
    BINOP2[(set! Var (binop Var Triv1))]
  Otherwise:
    (begin
      (set! u Triv)
      BINOP2[(set! u (binop u Triv2))
      (set! Var u))

  When Triv1 = Var, Triv2 != Var, it's tempting to produce:
    (begin
      MOVE[(set! Var Triv1)]
      BINOP2[(set! Var (binop Var Triv2)))
  but don't, since this sets up a new conflict between Var and Triv1,
  which can be a problem if they are both spilled since we don't rerun
  uncover-frame-conflict.  It would be okay if Triv1 is an integer or
  label, but it's probably better not to bother even in that case so
  that the computation is carried out in the register u even if Var
  ends up on the frame.

BINOP2[(set! Var (binop Var Triv))]

  binop = -, +, logor, logand

       ur   fvar   int32   int64/label
   ------------------------------------
    ur   |   ok    ok      ok  X1
    fvar |   ok    X1      ok  X1

    X1: (begin (set! u Triv) (set! Var (binop Var u)))

  binop = *
       ur   fvar   int32   int64/label
   ------------------------------------
    ur   |   ok    ok      ok  X1
    fvar |   X2    X2      X2  X2

    X2: (begin
    (set! u Var)
    BINOP2[(set! u (binop u Triv))]
    (set! Var u))

  binop = sra
    (set! Var (binop Var Triv))

RELOP[(relop Triv1 Triv2)

      ur   fvar   int32   int64/label
        ------------------------------------
  ur    |   ok    ok      ok   X6
  fvar  |   ok    X5      ok   X6
  int32       |   X4    X4      X5   X6
  int64/label |   X5    X5      X5   X7

  X4: (relop^ Triv2 Triv1)
  X5: (begin (set! u Triv1) (relop u Triv2))
  X6: (begin (set! u Triv2) (relop^ u Triv1))
  X7: (begin (set! u1 Triv1) (set! u2 Triv2) (relop u1 u2))
  relop^: (<, >), (>, <), (<=, >=), (>=, <=), (=, =)

ALTERNATE RELOP FORMULATION:

RELOP[(relop Triv1 Triv2)]
  Triv1 is a ur or fvar:
    RELOP2[(relop Triv1 Triv2)]
  Triv1 is not a ur or fvar, but Triv2 is
    RELOP2[(relop^ Triv2 Triv1)]
  Otherwise:
    (begin
      (set! u Triv1)
      RELOP2[(relop u Triv2)])

RELOP2[(relop Var Triv)]

      ur   fvar int32 int64/label
   ------------------------------------
  ur  |   ok   ok   ok   X8
  fvar  |   ok   X8   ok   X8

  X8: (begin (set! u Triv) (relop Var Triv))
|#

(define-who select-instructions
  (define (ur? x) (or (register? x) (uvar? x)))
  (define (Body x)
    (define new-ulocal* '())
    (define (new-u)
      (let ([u (unique-name 'u)])
        (set! new-ulocal* (cons u new-ulocal*))
        u))
    (define (select-binop-1 x op y z)
      (cond
        [(eq? y x) (select-binop-2 x op z)]
        [(and (eq? z x) (memq op '(* + logor logand))) (select-binop-2 x op y)]
        [else (let ([u (new-u)]) 
                (make-begin `((set! ,u ,y)
                              ,(select-binop-2 u op z)
                              (set! ,x ,u))))]))
    (define (select-binop-2 x op y)
      (case op
        [(- + logand logor)
         (if (or (and (ur? x) (or (ur? y) (frame-var? y) (int32? y)))
                 (and (frame-var? x) (or (ur? y) (int32? y))))
             `(set! ,x (,op ,x ,y))
             (let ([u (new-u)])
               (make-begin `((set! ,u ,y) (set! ,x (,op ,x ,u))))))]
        [(*)
         (if (ur? x)
             (if (or (ur? y) (frame-var? y) (int32? y))
                 `(set! ,x (,op ,x ,y))
                 (let ([u (new-u)])
                   (make-begin `((set! ,u ,y) (set! ,x (,op ,x ,u))))))
             (let ([u (new-u)])
               (make-begin
                 `((set! ,u ,x)
                   ,(select-binop-2 u op y)
                   (set! ,x ,u)))))]
        [(sra) `(set! ,x (,op ,x ,y))]
        [else (error who "unrecognized binop ~s" op)]))
    (define (select-move lhs rhs)
      (if (or (ur? lhs) (and (frame-var? lhs) (or (ur? rhs) (int32? rhs))))
          `(set! ,lhs ,rhs)
           (let ([u (new-u)])
             (make-begin `((set! ,u ,rhs) (set! ,lhs ,u))))))
   ; original formulation
#;
    (define (select-relop op x y)
      (define (swap-relop op)
        (cdr (assq op '((= . =) (< . >) (<= . >=) (> . <) (>= . <=)))))
      (cond
        [(or (and (ur? x) (or (ur? y) (frame-var? y) (int32? y)))
             (and (frame-var? x) (or (ur? y) (int32? y))))
         `(,op ,x ,y)]
        [(and (int32? x) (or (ur? y) (frame-var? y)))
         `(,(swap-relop op) ,y ,x)]
        [(or (and (frame-var? x) (frame-var? y))
             (and (int32? x) (int32? y))
             (and (or (and (int64? x) (not (int32? x))) (label? x))
                  (or (ur? y) (frame-var? y) (int32? y))))
         (let ([u (new-u)])
           (make-begin `((set! ,u ,x) (,op ,u ,y))))]
        [(and (or (ur? x) (frame-var? x) (int32? x))
              (or (and (int64? y) (not (int32? y))) (label? y)))
         (let ([u (new-u)])
           (make-begin `((set! ,u ,y) (,(swap-relop op) ,u ,x))))]
        [else
         (let ([u1 (new-u)] [u2 (new-u)])
           (make-begin `((set! ,u1 ,x) (set! ,u2 ,y) (,op ,u1 ,u2))))]))
   ; alternate formulation
    (define (select-relop-1 op x y)
       (define (swap-relop op)
         (cdr (assq op '((= . =) (< . >) (<= . >=) (> . <) (>= . <=)))))
       (cond
         [(or (ur? x) (frame-var? x)) (select-relop-2 op x y)]
         [(or (ur? y) (frame-var? y)) (select-relop-2 (swap-relop op) y x)]
         [else (let ([u (new-u)])
                 (make-begin `((set! ,u ,x)
                               ,(select-relop-2 op u y))))]))
    (define (select-relop-2 op x y)
      (if (if (ur? x)
              (or (ur? y) (frame-var? y) (int32? y))
              (or (ur? y) (int32? y)))
          `(,op ,x ,y)
          (let ([u (new-u)])
            (make-begin `((set! ,u ,y) (,op ,x ,u))))))
    (define (select-memop x y k)
      (cond
        [(or (ur? y) (int32? y))
         (if (ur? x)
             (k x y)
             (let ([u (new-u)])
               (make-begin `((set! ,u ,x) ,(k u y)))))]
        [(or (ur? x) (int32? x))
         (if (ur? y)
             (k y x)
             (let ([u (new-u)])
               (make-begin `((set! ,u ,y) ,(k u x)))))]
        [else (let ([u1 (new-u)] [u2 (new-u)])
                (make-begin `((set! ,u1 ,x) (set! ,u2 ,y) ,(k u1 u2))))]))
    (define (select-mref lhs x y)
      (if (ur? lhs)
          (select-memop x y (lambda (x y) `(set! ,lhs (mref ,x ,y))))
          (let ([u (new-u)])
            (make-begin
              `(,(select-memop x y (lambda (x y) `(set! ,u (mref ,x ,y))))
                (set! ,lhs ,u))))))
    (define (select-mset! x y z)
      (if (or (ur? z) (int32? z))
          (select-memop x y (lambda (x y) `(mset! ,x ,y ,z)))
          (let ([u (new-u)])
            (make-begin
              `((set! ,u ,z)
                ,(select-memop x y (lambda (x y) `(mset! ,x ,y ,u))))))))
    (define (Effect x)
      (match x
        [(nop) '(nop)]
        [(begin ,[ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
        [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(set! ,lhs (mref ,x ,y)) (select-mref lhs x y)]
        [(set! ,lhs (,binop ,x ,y)) (select-binop-1 lhs binop x y)]
        [(set! ,lhs ,rhs) (select-move lhs rhs)]
        [(mset! ,x ,y ,z) (select-mset! x y z)]
        [(return-point ,rplab ,[Tail -> tail]) `(return-point ,rplab ,tail)]
        [,x (error who "invalid Effect ~s" x)]))
    (define (Pred x)
      (match x
        [(true) '(true)]
        [(false) '(false)]
        [(begin ,[Effect -> ef*] ... ,[test]) (make-begin `(,ef* ... ,test))]
        [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(,relop ,x ,y) (select-relop-1 relop x y)]
        [,x (error who "invalid Pred ~s" x)]))
    (define (Tail x)
      (match x
        [(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,ef* ... ,tail))]
        [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(,triv ,live* ...) `(,triv ,live* ...)]
        [,x (error who "invalid Tail ~s" x)]))
    (match x
      [(locals (,local* ...) 
         (ulocals (,ulocal* ...)
           (locate (,home* ...) (frame-conflict ,ct ,[Tail -> tail]))))
       `(locals (,local* ...)
          (ulocals (,ulocal* ... ,new-ulocal* ...)
            (locate (,home* ...)
              (frame-conflict ,ct ,tail))))]
      [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
      [,x (error who "invalid Body ~s" x)]))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,x (error who "invalid Program ~s" x)])))

)