#lang play

#|
Nombre: Tomás Ubilla Zavala
¿Utilizó Whiteboard Policy? (SI o NO):
En caso afirmativo, ¿con quién?:
¿en qué ejercicio(s)?:
|#

#|
Methodology 
1. Understand what the function does
2. Write the function contract
3. Write the function purpose
4. Provide tests
5. Provide an implementation
|#

;;------------ ;;
;;==== P1 ==== ;;
;;------------ ;;

#| Parte A |#

#|
<poly> ::= (poly (Listof <number>))
         | (id <id>)
         | (add <poly> <poly>)
         | (mul <poly> <poly>)
         | (if0 <poly> <poly> <poly>)
         | (with <id> <poly> <poly>)
|#
;; Defines the recursive data type "poly" with its grammar.
(deftype Poly
  (poly l)
  (id x)
  (add r l)
  (mul r l)
  (if0 c r l)
  (with x r l)
)

#| Parte B |#

#|
<s-poly> ::= <number>
           | <symbol>
           | (list <number> ...)
           | (list + <s-poly> <s-poly> ...)
           | (list * <s-poly> <s-poly> ...)
           | (list if0 <s-poly> <s-poly> <s-poly>)
           | (list with <symbol> <s-poly> <s-poly>)
|#

;; remove-trailing-zeros :: (Listof Number) -> (Listof Number)
;; Removes trailing zeros from a list of numbers.
(define (remove-trailing-zeros lst)
  (define (helper l)
    (cond
      [(null? l) '()] ; si lista vacía, devuelve lista vacía temporalmente
      [else
       (let ((rest (helper (cdr l))))
         (if (and (null? rest) (= (car l) 0))
             '() ; eliminar cero al final
             (cons (car l) rest)))]))
  (let ((res (helper lst)))
    (if (null? res) '(0) res))) ; si toda la lista eran ceros, devuelve '(0)

;; parse :: <s-poly> -> Poly
;; Returns the parse source code into abstract syntax.
(define (parse s-poly)
  (match s-poly
    ;; atoms 
    [x #:when (number? x) (poly (list x))]
    [x #:when (symbol? x) (id x)]

    ;; operators
      ;; +
    [(list '+ first rest ...)
     (let* 
        ( 
          [f (parse first)] 
          [r (map parse rest)]
        )
        (foldl (lambda (v acc) (add acc v)) f r)
     )
    ]  
      ;; *          
    [(list '* first rest ...) 
     (let* 
        (
          [f (parse first)]
          [r (map parse rest)]
        )
        (foldl (lambda (v acc) (mul acc v)) f r)
      )
    ] 
    ;; if0
    [(list 'if0 c l r) (if0 (parse c) (parse l) (parse r))]
    
    ;; with
    [(list 'with x l r) (with x (parse l) (parse r))]
    
    ;; list of numbers (not empty)
    [x #:when 
      (and 
        (list? x) 
        (not (null? x)) 
        (andmap number? x)
      ) 
      (poly (remove-trailing-zeros x))] 
  )
)

#| Parte C |#

(deftype Env
  (tyEnv)
  (xtEnv symbol value env))

(define empty-env (tyEnv))

;; extend-env :: symbol Poly Env -> Env
;; Extiende un ambiente añadiendo una variable.
(define (extend-env x v env)
  (xtEnv x v env))

;; env-lookup :: symbol Env -> (or #f Env)
;; Busca una variable en un ambiente, retornando el
;; valor encontrado o #f en caso de que no esté definida.
(define (env-lookup x env)
  (match env
    [(tyEnv) #f]
    [(xtEnv s v tail) (if (symbol=? x s)
                          v
                          (env-lookup x tail))]))

;; PolySubst :: Poly Symbol Poly -> Poly
;; (PolySubst in what for) substitutes all free occurrences
;; of identifier 'what' in expression 'in' for expression 'for'.
(define (PolySubst in what for)
  (match in
    [(poly n) (poly n)]
    [(add l r) (add (PolySubst l what for) (PolySubst r what for))]
    [(mul l r) (mul (PolySubst l what for) (PolySubst r what for))]
    [(if0 c t f) (if0 (PolySubst c what for)
                      (PolySubst t what for)
                      (PolySubst f what for))]
    [(with x e b)
     (with x
           (PolySubst e what for)
           (if (symbol=? x what) ;; is x the identifier to be substituted? 
               b ;; x does not occur free in b
               (PolySubst b what for)))]
    [(id x) (if (symbol=? x what)
                for
                (id x))]))

;; reduce :: Poly Env -> Poly
(define (reduce p e)
  (match p
    [(poly l) (poly l)]
    [(add l r) 
      (match (list (reduce l e) (reduce r e))
        [(list (poly lst1) (poly lst2))
          (poly (map + lst1 lst2))
        ]
      )
    ]
    [(mul l r)       
      (match (list (reduce l e) (reduce r e))
        [(list (poly lst1) (poly lst2))
          (poly (map * lst1 lst2))
        ]
      )
    ]
    [(id x)
      (let ([val (env-lookup x e)])
        (if val
          val
          (error 'reduce (format "variable ~a is not defined" x))))
    ]
    [(if0 c t f)
      (if (equal? (poly '(0)) (reduce c e))
        (reduce t e)
        (reduce f e)
      )
    ]
    [(with x l r)
      (reduce 
        (PolySubst
          r
          x
          (reduce l e)
        )
        e
      )
    ]  
  )
)

;;------------ ;;
;;==== P2 ==== ;;
;;------------ ;;

#| Parte A |#

#|
<expr> ::= (real <number>)
        | (imaginary <number>)
        | (idc <idc>)
        | (addc <expr> <expr>)
        | (subc <expr> <expr>)
        | (if0c <expr> <expr> <expr>)
        | (withc (ListOf (PairOf <symbol> <expr>)) <expr>)
|#
(deftype Expr
  (real x)
  (imaginary x)
  (idc x)
  (addc l r)
  (subc l r)
  (if0c c t f)
  (withc l e)
)

#| Parte B |#

#|
Concrete syntax of expressions:

<s-expr> ::= <number>
           | (PairOf <number string>)
           | <symbol>
           | (+ <s-expr> <s-expr>)
           | (- <s-expr> <s-expr>)
           | (if0 <s-expr> <s-expr> <s-expr>)
           | (with (ListOf (PairOf <symbol> <number>)) s-expr)
|#

;; parser :: <s-expr> -> Expr

(define (parser s-expr) 
  (match s-expr
  ;; atoms
    [x #:when (number? x) (real x)]
    [x #:when (symbol? x) (idc x)]
    [x #:when 
      (and 
        (pair? x) 
        (number? (car x)) 
        (not(equal? (car x) 0)) 
        (string=? (symbol->string (cadr x)) "i")
      ) 
      (imaginary (car x))]

  ;; operators 
    ;; + -> addc
    [(list '+ l r) (addc (parser l) (parser r))]
    ;; - -> subc
    [(list '- l r) (subc (parser l) (parser r))]
    ;; if0 -> if0
    [(list 'if0c c l r) (if0 (parser c) (parser l) (parser r))]
    ;; with -> withc
    [(list 'with l x)
      (if (zero? (length l))
        "parser: *with* expects at least one definition"
        (withc
          (map (lambda (s) (cons (car s) (parser (cadr s)))) l)
          (parser x)))]
  )
)

#| Parte C |#

#|
<cvalue> ::= (compV <num> <num>)
|#

(deftype CValue (compV r i))

;; from-CValue :: CValue -> Expr
(define (from-CValue v) 
  (def x (compV-r v))
  (def y (compV-i v))
  (addc (real x) (imaginary y))
)

;; cmplx+ :: CValue CValue -> CValue
(define (cmplx+ v1 v2) 
  (def (compV r1 im1) v1) ;; cast
  (def (compV r2 im2) v2) ;; cast
  (compV (+ r1 r2) (+ im1 im2))
)

;; cmplx- :: CValue CValue -> CValue
(define (cmplx- v1 v2)
  (def (compV r1 im1) v1) ;; cast
  (def (compV r2 im2) v2) ;; cast
  (compV (- r1 r2) (- im1 im2))
)

;; cmplx0? :: CValue -> Boolean
(define (cmplx0? v)
  (def (compV re im) v)
  (and (zero? re) (zero? im))
)  

#| Parte D |#

;; subst :: Expr Symbol Expr -> Expr
(define (subst in what for)
  (match in
    [(real n) (real n)]
    [(imaginary n) (imaginary n)]
    [(idc x) (if (symbol=? x what)
                 for
                 (idc x))]
    [(addc l r) (addc (subst l what for)
                      (subst r what for))]
    [(subc l r) (subc (subst l what for)
                      (subst r what for))]
    [(if0c c t f) (if0c (subst c what for)
                        (subst t what for)
                        (subst f what for))]
    [(withc defs body)
      ;; Función auxiliar para manejar shadowing en defs
      (define (subst-defs ds)
        (cond
          [(empty? ds) (values '() #f)]
          [else
           (define name (car (car ds)))
           (define expr (cdr (car ds)))
           (if (symbol=? name what)
               ;; Encontramos shadowing
               (values (cons (cons name expr) (cdr ds)) #t)
               (let-values ([(rest-defs shadow?) (subst-defs (cdr ds))])
                 (values (cons (cons name (subst expr what for)) rest-defs)
                         shadow?)))]))
      (define-values (new-defs shadow?) (subst-defs defs))
      (withc new-defs
        (if shadow?
            body
            (subst body what for)))]))

#| Parte E |#

;; interp :: Expr -> CValue
(define (interp expr) 
  (match expr
    [(real x) (compV x 0)]
    [(imaginary x) (compV 0 x)]
    [(idc x) (error "Free occurrence of a variable")]
    [(addc l r) (cmplx+ (interp l) (interp r))]
    [(subc l r) (cmplx- (interp l) (interp r))]
    [(if0c c t f) 
      (if (cmplx0? (interp c))
          (interp t)
          (interp f))]
    [(withc defs body)
      ;; Evaluamos las defs secuencialmente y sustituimos en el body
      (let loop ([remaining defs] [accum '()])
        (if (null? remaining)
            ;; Aplicamos todas las bindings acumuladas al cuerpo
            (interp
              (foldl (lambda (b d)
                       (subst b (car d) (cdr d)))
                     body
                     accum))
            (let* ([d (car remaining)]
                   [name (car d)]
                   [expr (cdr d)]
                   ;; sustituimos solo las bindings previas en expr
                   [expr-sub (foldl (lambda (e d2)
                                      (subst e (car d2) (cdr d2)))
                                    expr
                                    accum)]
                   [accum2 (append accum (list (cons name expr-sub)))])
              (loop (cdr remaining) accum2))))]))

