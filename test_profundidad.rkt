#lang play
(require "base.rkt")

; test typeof:
(test (typeof (parse 6)(mtTEnv))(list (TNum)))
(test (typeof (parse '{+ {+ {- 6 5} 4} 6})(mtTEnv))(list (TNum) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))))
(test (typeof (parse '{+ 10 12})(mtTEnv)) (list (TNum) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))))
(test (typeof (parse 'x)(anTEnv 'x (TNum) (mtTEnv)))(list (TNum)))
(test (typeof (parse '{- 10 x})(anTEnv 'x (TVar 1)  (mtTEnv)))(list (TNum) (Cnst (TNum) (TNum)) (Cnst (TVar 1) (TNum))))
(test/exn (typeof (fun 'x (id 'y))(mtTEnv)) "free identifier y")
(reset)
; '{+ 4 {with {x 10} {+ x 5}}}
(test (typeof (add (num 4) (app (fun 'x (add (id 'x) (num 5))) (num 10))) (mtTEnv)) (list (TNum) (Cnst (TVar 1) (TNum))(Cnst (TNum) (TNum))(Cnst (TFun (TVar 1) (TNum)) (TFun (TNum) (TVar 2)))(Cnst (TNum) (TNum)) (Cnst (TVar 2) (TNum))))
(reset)
; '{with {x 5} {with {y 10} {with {z 3} {+ {+ x y} z}}}}
(test (typeof (app (fun 'x (app (fun 'y (app (fun 'z (add (add (id 'x) (id 'y)) (id 'z))) (num 3))) (num 10))) (num 5)) (mtTEnv)) (list
 (TVar 6)
 (Cnst (TVar 1) (TNum))
 (Cnst (TVar 2) (TNum))
 (Cnst (TNum) (TNum))
 (Cnst (TVar 3) (TNum))
 (Cnst (TFun (TVar 3) (TNum)) (TFun (TNum) (TVar 4)))
 (Cnst (TFun (TVar 2) (TVar 4)) (TFun (TNum) (TVar 5)))
 (Cnst (TFun (TVar 1) (TVar 5)) (TFun (TNum) (TVar 6)))))

(test/exn (typeof (id 'x) (mtTEnv)) "free identifier x")
(reset)

