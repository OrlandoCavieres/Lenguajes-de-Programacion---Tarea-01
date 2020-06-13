#lang play
;################################ Interprete visto en clases ###########################

; Expr
; <Expr> ::= <num>
;          | {fun {<id>} <Expr>}
;          | {+ <Expr> <Expr>}
;          | {- <Expr> <Expr>}
;          | {with {<id> <Expr>} <Expr>}
;          | <id>
;          | {<Expr> <Expr>}
;          | {if0 <Expr> <Expr> <Expr>}

(deftype Expr
  (num n)
  (fun param body)
  (add l r)
  (sub l r)
  (id x)
  (app fun-expr arg-expr)
  (if0 e tb fb))

(deftype Val
  (numV n)
  (closureV param body env))

(define (parse s-expr)
  (match s-expr
    [(? number?) (num s-expr)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c)
                            (parse t)
                            (parse f))]
    [(list 'with (list id named-expr) body) (app (fun id (parse body)) (parse named-expr))]
    [(? symbol?) (id s-expr)]
    [(list 'fun (list id) body) (fun id (parse body))]
    [(list fun-expr arg-expr) (app (parse fun-expr) (parse arg-expr))] 
    ))

(define (operate f n1 n2)
  (numV (f (numV-n n1) (numV-n n2))))

(deftype Env
  (mtEnv)
  (anEnv id val env))

(define (lookup-env x env)
  (match env
    [(mtEnv) (error "identificador libre!!" x)]
    [(anEnv id val restoEnv)
     (if (symbol=? id x)
         val
         (lookup-env x restoEnv))]))

(define (interp expr env)
  (match expr
    [(num n) (numV n)]
    [(add l r) (operate + (interp l env) (interp r env))]
    [(sub l r) (operate - (interp l env) (interp r env))]
    [(if0 c t f)
     (if (zero? (interp c env))
         (interp t  env)
         (interp f env))]
    [(id s) (lookup-env s env)]
    [(fun id body) (closureV id body env)]  
    [(app fun-expr arg-expr)
     (def fun-val (interp fun-expr env))
     (interp (closureV-body fun-val)
              (anEnv (closureV-param fun-val)
                     (interp arg-expr env) (closureV-env fun-val)))
     ]))

(define (run prog)
  (interp (parse prog) (mtEnv)))


;################################ Definiciones ###########################

(deftype Type
  (TNum)
  (TFun Targ Tret)
  (TVar Symbol))

(deftype Constraint
  (Cnst T1 T2))

(deftype TEnv
  (mtTEnv)
  (anTEnv id Type env))

(define count 0)

(define (get-id)
  (begin
    (set! count (add1 count))
    count))

(define (reset)
  (set! count 0))

(define (prettyfy T)
  (match T
    [(TNum) "num"]
    [(TVar x) (string-append "(TVar " x ")")]
    [(TFun T1 T2) (string-append "(TFun " (prettyfy T1) " " (prettyfy T2) ")")]))


;################################ Su código va aquí ###########################

(define (lookupT-env x Tenv)
  (match Tenv
    [(mtTEnv) (error "identificador libre!!" x)]
    [(anTEnv id Type restoTEnv)
     (if (symbol=? id x)
         Type
         (lookupT-env x restoTEnv))]))

(define (obtenerTipo expr Tenv)
  (match expr
    [(num n) (list (TNum) Tenv)]
    [(add izquierda derecha) (list (TNum) Tenv)]
    [(sub izquierda derecha) (list (TNum) Tenv)]
    [(id identificador) (list (lookupT-env identificador Tenv) Tenv)]
    [(if0 e tb fb) (list (car (obtenerTipo tb Tenv)) Tenv)]
  )
)

(define (obtenerConstrains expr Tenv)
  (match expr
    [(num n) '()]
    [(id identificador) '()]
    [(add izquierda derecha)
      (append
        (obtenerConstrains izquierda Tenv)
        (obtenerConstrains derecha Tenv)
        (list (Cnst (car (obtenerTipo izquierda Tenv)) (TNum))
              (Cnst (car (obtenerTipo derecha Tenv)) (TNum))
        )
      )
    ]
    [(sub izquierda derecha)
      (append
        (obtenerConstrains izquierda Tenv)
        (obtenerConstrains derecha Tenv)
        (list (Cnst (car (obtenerTipo izquierda Tenv)) (TNum))
              (Cnst (car (obtenerTipo derecha Tenv)) (TNum))
        )
      )
    ]
    [(if0 condicion trueTense falseTense)
      (append
        (obtenerConstrains condicion Tenv)
        (obtenerConstrains trueTense Tenv)
        (obtenerConstrains falseTense Tenv)
        (list (Cnst (car (obtenerTipo condicion Tenv)) (TNum))
              (Cnst (car (obtenerTipo trueTense Tenv)) (car (obtenerTipo falseTense Tenv)))
        )
      )
    ]
  )
)

(define (typeof expr Tenv)
  (append (list (car (obtenerTipo expr Tenv))) (obtenerConstrains expr Tenv))
)

(define (substitute tvar type list)
  (void))

(define (unify _list)
  (void))

(define (lookup-list _list var)
  (void))

(define (runType s-expr)
  (void))