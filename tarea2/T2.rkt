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

;; reduce :: Poly Env -> Poly



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
            (parser (car x))))]
  )
)

#| Parte C |#

#|
<cvalue> ::= (compV <num> <num>)
|#

(deftype CValue (compV r i))

;; from-CValue :: CValue -> Expr
(define (from-CValue v) '???)

;; cmplx+ :: CValue CValue -> CValue
(define (cmplx+ v1 v2) '???)

;; cmplx- :: CValue CValue -> CValue
(define (cmplx- v1 v2) '???)

;; cmplx0? :: CValue -> Boolean
(define (cmplx0? v) '???)

#| Parte D |#

;; subst :: Expr Symbol Expr -> Expr
(define (subst in what for) '???)

#| Parte E |#

;; interp :: Expr -> CValue
(define (interp expr) '???)

