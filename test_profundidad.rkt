#lang play
(require "base.rkt")

; test typeof:
(test (typeof (parse 6)(mtTEnv)) (list (TNum)))
(test (typeof (num 3)(mtTEnv)) (list (TNum)))
(test (typeof (parse '{+ {+ {- 6 5} 4} 6})(mtTEnv)) (list (TNum) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))))
(test (typeof (parse '{+ 10 12})(mtTEnv)) (list (TNum) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))))
(test (typeof (add (num 10) (num 3)) (mtTEnv)) ( list ( TNum ) ( Cnst ( TNum ) ( TNum )) ( Cnst ( TNum ) ( TNum ))))
(test (typeof (parse 'x)(anTEnv 'x (TNum) (mtTEnv))) (list (TNum)))
(reset)
(test (typeof (parse '{- 10 x}) (anTEnv 'x (TVar 1) (mtTEnv))) (list (TNum) (Cnst (TNum) (TNum)) (Cnst (TVar 1) (TNum))))
(reset)
(test (typeof (add (id 'z) (num 100)) (anTEnv 'z (TVar 1) (mtTEnv))) (list (TNum) (Cnst (TVar 1) (TNum)) (Cnst (TNum) (TNum))))
(reset)
(test/exn (typeof (fun 'x (id 'y)) (mtTEnv)) "identificador libre!! y")
(reset)
(test (typeof (fun 'x (add (id 'x) (num 1))) (mtTEnv))(list (TFun (TVar 1) (TNum)) (Cnst (TVar 1) (TNum)) (Cnst (TNum) (TNum))))
(reset)
; '{+ 4 {with {x 10} {+ x 5}}}
(test (typeof (add (num 4) (app (fun 'x (add (id 'x) (num 5))) (num 10))) (mtTEnv)) (list (TNum) (Cnst (TVar 1) (TNum))(Cnst (TNum) (TNum))(Cnst (TFun (TVar 1) (TNum)) (TFun (TNum) (TVar 2)))(Cnst (TNum) (TNum)) (Cnst (TVar 4) (TNum))))
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
(reset)

(test (typeof (app (fun 'x (id 'x)) (num 3)) (mtTEnv)) (list (TVar 2) (Cnst (TFun (TVar 1) (TVar 1)) (TFun (TNum) (TVar 2)))))
(reset)

(test/exn (typeof (id 'x) (mtTEnv)) "identificador libre!! x")
(reset)

(test (typeof (if0 (num 2) (num 5) (num 3)) (mtTEnv)) (list (TNum) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))))
