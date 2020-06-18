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
    [(TVar x) (string-append "(TVar " (number->string x) ")")]
    [(TFun T1 T2) (string-append "(TFun " (prettyfy T1) " " (prettyfy T2) ")")]))


;################################ Su código va aquí ###########################

; Definiciones obtenidas del libro 
; "Programming Languages: Application and Interpretation" de Shriram Krishnamurthi
; Se abstrayeron los algoritmos para formalizar todas las funciones y definiciones solicitadas.
; Se obtuvo la metodología de comparación, usos de boolenos, uso de funciones de chequeo, manejos de variables
; en chequeo e inferencia de invariantes estáticos o tipos en entornos, desde el libro mencionado 
; (Capitulo 14 - 15), la documentación de referencias de racket y el lenguaje play en su respectiva pagina web,
; y paginas de manejo de tipos en racket como https://courses.cs.washington.edu/courses/cse341/19sp/unit6notes.pdf.
; También se recurrió a la idea de interprete de tipos en libros de introducción a lenguajes de programación en 
; otros lenguajes como Haskell, Scheme, ML, Scala, Perl y Python.

(define (lookupT-env x env)
  (match env
    [(mtTEnv) (error 'Exception "Identificador libre = ~a" x)]
    [(anTEnv id Type restoTEnv)
     (if (equal? id x)
         Type
         (lookupT-env x restoTEnv))]))

(define (typeof expr Tenv)
  (match expr
    [(num n) (list (TNum))]
    [(id identificador) (list (lookupT-env identificador Tenv))]
    [(add izquierda derecha)
      (append
        (list (TNum))
        (cdr (typeof izquierda Tenv))
        (cdr (typeof derecha Tenv))
        (list (Cnst (car (typeof izquierda Tenv)) (TNum))
              (Cnst (car (typeof derecha Tenv)) (TNum))
        )
      )
    ]
    [(sub izquierda derecha)
      (append
        (list (TNum))
        (cdr (typeof izquierda Tenv))
        (cdr (typeof derecha Tenv))
        (list (Cnst (car (typeof izquierda Tenv)) (TNum))
              (Cnst (car (typeof derecha Tenv)) (TNum))
        )
      )
    ]
    [(if0 condicion trueTense falseTense)
      (append 
        (list (car (typeof trueTense Tenv)))
        (cdr (typeof condicion Tenv))
        (cdr (typeof trueTense Tenv))
        (cdr (typeof falseTense Tenv))
        (list (Cnst (car (typeof condicion Tenv)) (TNum))
              (Cnst (car (typeof trueTense Tenv)) (car (typeof falseTense Tenv)))
        )
      )
    ]
    [(fun argumento cuerpoFuncion)
      (define tipoVar (TVar (get-id)))
      (define nuevoEnv (anTEnv argumento (TVar count) Tenv))
      (define nuevoCuerpo (typeof cuerpoFuncion nuevoEnv))
      (append (list (TFun (lookupT-env argumento nuevoEnv) (car nuevoCuerpo))) (cdr nuevoCuerpo))
    ]
    [(app fun parametro)
      (define tipoFuncion (typeof fun Tenv))
      (define tipoParametro (typeof parametro Tenv))
      (define tipoVar (TVar (get-id)))
      (define nuevoAppEnv (anTEnv (app fun parametro) (TVar count) Tenv))
      (define tipoApp (lookupT-env (app fun parametro) nuevoAppEnv))
      (append
        (list tipoVar)
        (cdr tipoFuncion)
        (cdr tipoParametro)
        (list (Cnst (car tipoFuncion) (TFun (car tipoParametro) tipoApp)))
      )
    ]
  )
)

; Para realizar las funciones que comparan tipos, las funciones que verifican si una variable es de un tipo 
; especifico, función de unificación y reemplazo de tipo, se recurrió a bibliografía nuevamente, entre ellas
; el libro "Type Systemsfor Programming Languages" de Pierce en sus capitulos sobre definicion de tipos, 
; equivalencias y unificación bajo inferencia de tipo (http://ropas.snu.ac.kr/~kwang/520/pierce_book.pdf)
; y la wikipedia para el procedimiento de tipación y unificación, asi como el manejo de listas y contrains
; en un lenguaje funcional para determinar un tipo final.
; https://en.wikipedia.org/wiki/Type_inference  https://en.wikipedia.org/wiki/Type_system  
; https://en.wikipedia.org/wiki/Unification_(computer_science)
; https://en.wikipedia.org/wiki/Occurs_check 
; http://scheme2006.cs.uchicago.edu/13-siek.pdf 


(define (compararTipoConstrain tipoExpresion TVar_aBuscar tipoParaReemplazar)
  (match tipoExpresion
    [(TNum) (TNum)]
    [(TVar numero) 
      (if 
        (match tipoExpresion
          [(TVar primerNumero) 
            (match TVar_aBuscar
              [(TVar segundoNumero) (equal? primerNumero segundoNumero)] 
            )
          ]
        )
        tipoParaReemplazar
        tipoExpresion
      )
    ]
    [(TFun entrada salida) 
      (TFun (compararTipoConstrain entrada TVar_aBuscar tipoParaReemplazar) 
            (compararTipoConstrain salida TVar_aBuscar tipoParaReemplazar)
      )
    ]
  )
)

(define (substitute TVar_aBuscar tipoParaReemplazar listaConstrains)
  (if (empty? listaConstrains)
    listaConstrains
    (append 
      (list 
        (match (car listaConstrains)
          [(Cnst tipoExpresion1 tipoExpresion2) 
            (Cnst (compararTipoConstrain tipoExpresion1 TVar_aBuscar tipoParaReemplazar) 
                  (compararTipoConstrain tipoExpresion2 TVar_aBuscar tipoParaReemplazar)
            )
          ]
        )
      )
      (substitute TVar_aBuscar tipoParaReemplazar (cdr listaConstrains))
    )
  )
)

(define (esTNum? tipoVariable)
  (match tipoVariable
    [(TNum) #t]
    [(TVar numero) #f]
    [(TFun entrada salida) #f]
  )
)

(define (esTVar? tipoVariable)
  (match tipoVariable
    [(TNum) #f]
    [(TVar numero) #t]
    [(TFun entrada salida) #f]
  )
)

(define (esTFun? tipoVariable)
  (match tipoVariable
    [(TNum) #f]
    [(TVar numero) #f]
    [(TFun entrada salida) #t]
  )
)

(define (esSubExpresion? tvar tipo)
  (match tipo
    [(TNum) #f]
    [(TVar n) 
      (match tvar
        [(TVar primerNumero) 
          (match tipo
            [(TVar segundoNumero) (equal? primerNumero segundoNumero)]
          )
        ]
      )
    ]
    [(TFun tipoEntrada tipoSalida)
      (or (esSubExpresion? tvar tipoEntrada) (esSubExpresion? tvar tipoSalida))
    ]
  )
)

(define (compararTipoEntreDosExpresiones primerTipo segundoTipo)
  (match primerTipo
    [(TNum) (esTNum? segundoTipo)]
    [(TVar primerNumero)
      (if (esTVar? segundoTipo)
        (match segundoTipo
          [(TVar segundoNumero) (equal? primerNumero segundoNumero)]
        )
        #f
      )
    ]
    [(TFun T1entrada T1salida)
      (if (esTFun? segundoTipo)
        (match segundoTipo
          [(TFun T2entrada T2salida) 
            (and (compararTipoEntreDosExpresiones T1entrada T2entrada) (compararTipoEntreDosExpresiones T1salida T2salida))
          ]
        )
        #f
      )
    ]
  )
)

(define (unify listaConstrains)
  (if (empty? listaConstrains)
    empty
    (let* ([cabeza (car listaConstrains)]
           [cola (cdr listaConstrains)])  
      (match cabeza
        [(Cnst termino1 termino2)
          (if (compararTipoEntreDosExpresiones termino1 termino2)
            (unify cola)
            (if (and (esTVar? termino1) (not (esSubExpresion? termino1 termino2)))
              (append
                (unify (substitute termino1 termino2 cola))
                (list (Cnst termino1 termino2))
              )
              (if (and (esTVar? termino2) (not (esSubExpresion? termino2 termino1)))
                (append
                  (unify (substitute termino2 termino1 cola))
                  (list (Cnst termino2 termino1))
                )
                (if (and (esTFun? termino1) (esTFun? termino2))
                  (match termino1
                    [(TFun entradaTermino1 salidaTermino1)
                      (match termino2
                        [(TFun entradaTermino2 salidaTermino2)
                          (unify 
                            (append
                              cola
                              (list (Cnst entradaTermino1 entradaTermino2)
                                    (Cnst salidaTermino1 salidaTermino2)
                              )
                            )
                          )
                        ]
                      )  
                    ]
                  )
                  (error 'Exception "Error de Tipo: No se puede unificar [~a] con [~a]" (prettyfy termino1) (prettyfy termino2))
                )
              )
            )
          )
        ]
      )
    )  
  )
)

(define (lookup-list listaConstrains variableTipo)
  (if (empty? listaConstrains)
    variableTipo
    (let* ([cabeza (car listaConstrains)]
           [cola (cdr listaConstrains)])
      (if (esTNum? variableTipo)
        variableTipo
        (if (esTFun? variableTipo)
          (match variableTipo
            [(TFun entrada salida) (TFun (lookup-list listaConstrains entrada) (lookup-list listaConstrains salida))]
          )
          (match cabeza
            [(Cnst termino1 termino2)
              (if (compararTipoEntreDosExpresiones termino1 variableTipo)
                (lookup-list listaConstrains termino2)
                (lookup-list cola variableTipo)
              )
            ]
          )
        )
      )
    )
  )
)
  
(define (runType s-expr)
  (define resultadoFuncionTypeof (typeof (parse s-expr) (mtTEnv)))
  (define tipoExpresion (car resultadoFuncionTypeof))
  (define constrainsExpresion (cdr resultadoFuncionTypeof))
  (define listaContrainsProcesadoConUnify (unify constrainsExpresion))
  (lookup-list listaContrainsProcesadoConUnify tipoExpresion)
)