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

#|  Definiciones obtenidas del libro 
    "Programming Languages: Application and Interpretation" de Shriram Krishnamurthi
    Se abstrayeron los algoritmos para formalizar todas las funciones y definiciones solicitadas.
    Se obtuvo la metodología de comparación, usos de boolenos, uso de funciones de chequeo, manejos de variables
    en chequeo e inferencia de invariantes estáticos o tipos en entornos, desde el libro mencionado 
    (Capitulo 14 - 15), la documentación de referencias de racket y el lenguaje play en su respectiva pagina web,
    y paginas de manejo de tipos en racket como https://courses.cs.washington.edu/courses/cse341/19sp/unit6notes.pdf.
    También se recurrió a la idea de interprete de tipos en libros de introducción a lenguajes de programación en 
    otros lenguajes como Haskell, Scheme, ML, Scala, Perl y Python. |#


#|  lookupT-env :: id Tenv -> Type
    Función que dado un identificador (id) y un ambiente de tipos (Tenv), retorna el tipo asociado (Type) 
    al identificador. 
    Ejemplo: (lookupT-env 'x (anTEnv 'x (TVar 1) (mtTEnv))) -> (TVar 1)|#
(define (lookupT-env x Tenv)
  (match Tenv
    [(mtTEnv) (error 'Exception "Identificador libre = ~a" x)]
    [(anTEnv id Type restoTEnv)
     (if (equal? id x)
         Type
         (lookupT-env x restoTEnv))]))

#|  typeof :: expr TEnv -> list[Type, list[Constraints]]
    Función que dada una expresion (expr) y un ambiente de tipos (TEnv), retorna el tipo de la expresión
    con la lista de constraints que debe ser solucionada para que el programa sea válido en tipos. La
    función reporta errores solo en caso de identificadores libres. 
    Es una función recursiva que en cada llave de posibilidad de la expresión establece una relación de tipo
    base y de los contraints necesarios de solucionar para aquel tipo. Para ello emplea la estructura constraint
    y puede añadir variables al ambiente de tipos (Tenv) al llamar a la funcion lookupT-env.
    Retorna una lista cuyo primer elemento es el posible tipo final de la expresión y su lista de constraints
    concatenada. 
    Ejemplo: (typeof (add (num 10) (num 3)) (mtTEnv)) -> (list (TNum) (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))) |#
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
; el libro "Type Systems for Programming Languages" de Pierce en sus capitulos sobre definicion de tipos, 
; equivalencias y unificación bajo inferencia de tipo (http://ropas.snu.ac.kr/~kwang/520/pierce_book.pdf)
; y la wikipedia para el procedimiento de tipación y unificación, asi como el manejo de listas y contrains
; en un lenguaje funcional para determinar un tipo final.
; https://en.wikipedia.org/wiki/Type_inference  https://en.wikipedia.org/wiki/Type_system  
; https://en.wikipedia.org/wiki/Unification_(computer_science)
; https://en.wikipedia.org/wiki/Occurs_check 
; http://scheme2006.cs.uchicago.edu/13-siek.pdf 

#|  compararyReemplazarTipoContrain :: Type1 TVar Type2 -> Type
    Función que a partir de un TVar a buscar, el tipo de una expresión (Type (1)) y un tipo a reemplazar 
    (Type (2)), determina si el TVar es igual al tipo de la expresión y si lo es, retorna el tipo a reemplazar.
    Caso contrario retorna el tipo de la expresión. 
    La función hace recursión si el tipo de la expresión es TFun, ya que debe buscar en su interior si 
    existe un TVar válido para reemplazo. En caso que sea TNum se retorna TNum, ya que no sería un TVar que 
    necesite de reemplazo.
    Ejemplo: (compararyReemplazarTipoConstrain (TVar 2) (TVar 2) (TNum)) -> (TNum) |#
(define (compararyReemplazarTipoConstrain tipoExpresion TVar_aBuscar tipoParaReemplazar)
  (match tipoExpresion
    [(TNum) (TNum)]
    [(TVar primerNumero) 
      (if 
        (match TVar_aBuscar
              [(TVar segundoNumero) (equal? primerNumero segundoNumero)] 
        )
        tipoParaReemplazar
        tipoExpresion
      )
    ]
    [(TFun entrada salida) 
      (TFun (compararyReemplazarTipoConstrain entrada TVar_aBuscar tipoParaReemplazar) 
            (compararyReemplazarTipoConstrain salida TVar_aBuscar tipoParaReemplazar)
      )
    ]
  )
)

#|  substitute :: TVar Type List[Constraints] -> List[Constraints]
    Función que reemplaza todas las ocurrencias de TVar en List[Constraints] de entrada por el Type señalado,
    retornando una nueva lista List[Constraints] con todos los TVar señalados presentes en ella originalmente
    sustituidos.
    La función avanza recursivamente por List[Constraints] comparando los tipos asociados al constraint de la
    cabeza con el TVar a reemplazar y reemplazandolo por el Type, al emplear la función auxiliar asociada
    compararyReemplazarTipoConstraint. Una vez finaliza de reemplazar, se genera un nuevo constraint, el que es 
    añadido a una nueva lista, la que finalmente es retornada como resultado.
    Ejemplo: (substitute (TVar 1) (TNum) (list (Cnst (TNum) (TNum)) (Cnst (TVar 1) (TNum)))) ->
             (list (Cnst (TNum) (TNum)) (Cnst (TNum) (TNum))) |#
(define (substitute TVar_aBuscar tipoParaReemplazar listaConstrains)
  (if (empty? listaConstrains)
    listaConstrains
    (append 
      (list 
        (match (car listaConstrains)
          [(Cnst tipoExpresion1 tipoExpresion2) 
            (Cnst (compararyReemplazarTipoConstrain tipoExpresion1 TVar_aBuscar tipoParaReemplazar) 
                  (compararyReemplazarTipoConstrain tipoExpresion2 TVar_aBuscar tipoParaReemplazar)
            )
          ]
        )
      )
      (substitute TVar_aBuscar tipoParaReemplazar (cdr listaConstrains))
    )
  )
)

#|  esSubexpresion? :: TVar Type -> bool
    Función que verifica si un TVar se encuentra como un subtipo de otra expresión de tipos.
    La función es recursiva recorriendo la expresion desde lo más externo hacia lo más interno.
    Si el TVar se encuentra dentro de otra expresion o es igual, entonces retorna #t, caso contrario
    retorna #f.
    Ejemplo: (esSubexpresion? (TVar 1) (TNum)) -> #f |#
(define (esSubExpresion? tvar tipo)
  (match tipo
    [(TNum) #f]
    [(TVar primerNumero) 
      (match tvar
        [(TVar segundoNumero) (equal? primerNumero segundoNumero)]
      )
    ]
    [(TFun tipoEntrada tipoSalida)
      (or (esSubExpresion? tvar tipoEntrada) (esSubExpresion? tvar tipoSalida))
    ]
  )
)

#|  compararTipoEntreDosExpresiones :: Type Type -> bool
    Función que compara dos tipos para ver si son iguales o no.
    Verifica si ambos son TNum o no, si ambos son Tvar o no, o si ambos son TFun o no. En el caso 
    de que ambas sean TFun, se produce recursión en la función, realizando así la misma comparación entre
    los parametros del tipo TFun que deber ser iguales.
    Ejemplo: (compararTipoEntreDosExpresiones (TVar 1) (TVar 1)) -> #t  |#
(define (compararTipoEntreDosExpresiones primerTipo segundoTipo)
  (match primerTipo
    [(TNum) (TNum? segundoTipo)]
    [(TVar primerNumero)
      (if (TVar? segundoTipo)
        (match segundoTipo
          [(TVar segundoNumero) (equal? primerNumero segundoNumero)]
        )
        #f
      )
    ]
    [(TFun T1entrada T1salida)
      (if (TFun? segundoTipo)
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

#|  unify :: list[Constraints] -> list[Constraints]
    Función que dada una lista de constraints retorna una lista "unificada" de constraints, que consiste
    en una lista simplificada de los constraints presentes en la lista original, donde se encuentran mejor
    relacionados los tipos involucrados.
    La función emplea funciones auxiliares de verificación de tipo booleana tales como TVar?, TFun? y 
    esSubexpresion?, así como la función de comparación recursiva compararTipoEntreDosExpresiones.
    La función es recursiva, avanzando por la lista de constraints y analizando cada uno de sus elementos
    para establecer constraints nuevos, cambiando tipos conocidos y así simplificando la lista original.
    Ejemplo: (unify (list (Cnst (TNum) (TNum)) (Cnst (TVar 2) (TNum)))) -> (list (Cnst (TVar 2) (TNum))) |#
(define (unify listaConstrains)
  (if (empty? listaConstrains)
    empty
    (let* ([cabeza (car listaConstrains)]
           [cola (cdr listaConstrains)])  
      (match cabeza
        [(Cnst termino1 termino2)
          (if (compararTipoEntreDosExpresiones termino1 termino2)
            (unify cola)
            (if (and (TVar? termino1) (not (esSubExpresion? termino1 termino2)))
              (append
                (unify (substitute termino1 termino2 cola))
                (list (Cnst termino1 termino2))
              )
              (if (and (TVar? termino2) (not (esSubExpresion? termino2 termino1)))
                (append
                  (unify (substitute termino2 termino1 cola))
                  (list (Cnst termino2 termino1))
                )
                (if (and (TFun? termino1) (TFun? termino2))
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

#|  loockup-list :: Lista[Constrains] Type(1) -> Type(2)
    Función que a partir de una lista de constrains (Lista[contrains]) y una variable tipo (Type (1)), busca en la lista 
    el tipo asociado (Type (2)) a la variable tipo (Type (1)). Si la lista esta vacia, retorna la variable tipo ingresada
    como parametro (Type (1)).
    La función avanza recursivamente por la lista de constrains, empleando funciones verificadores de tipo para TNum, TVar o
    TFun, y buscando la variable tipo (Type (1)) dentro de funciones y del entorno de variables mediante su id. 
    ejemplo: (lookup-list '() (TNum)) -> (TNum) |#
(define (lookup-list listaConstrains variableTipo)
  (if (empty? listaConstrains)
    variableTipo
    (let* ([cabeza (car listaConstrains)]
           [cola (cdr listaConstrains)])
      (if (TNum? variableTipo)
        variableTipo
        (if (TFun? variableTipo)
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
#|  RunType :: S-Expr -> Type
    Función que dada una expresión en sintasix concreta retorna su tipo final calculado con las funciones parse,
    typeof, unify y lookup-list. Si no se cumplen las condiciones correctas entonces también puede retornar un error.
    Ejemplo: (runType '{with {y 20} {- 40 y}}) -> (TNum) |#
(define (runType s-expr)
  (define resultadoFuncionTypeof (typeof (parse s-expr) (mtTEnv)))
  (define tipoExpresion (car resultadoFuncionTypeof))
  (define constrainsExpresion (cdr resultadoFuncionTypeof))
  (define listaContrainsProcesadoConUnify (unify constrainsExpresion))
  (lookup-list listaContrainsProcesadoConUnify tipoExpresion)
)