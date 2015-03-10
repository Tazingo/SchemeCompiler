(import (Compiler compile))
(import (Framework testing))

(define test 
  '(letrec ([f$0 (lambda (p.2 i.3 i.4) (- (mref p.2 i.3) (mref p.2 i.4)))])
      (let ([x.1 (alloc 16)])
        (begin
          (mset! x.1 0 73)
          (mset! x.1 8 35)
          (+ (f$0 x.1 0 8) -41))))
  )

(display (p423-compile test))
(newline)
(exit)



