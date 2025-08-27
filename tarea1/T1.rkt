#lang play
#|
¿Utilizó Ud. la política de Whiteboard Policy para la resolución de la tarea (complete con SI o NO): NO
En caso afirmativo, indique con quién y sobre qué ejercicio(s): 
|#

#|
Methodology 
1. Understand what the function does
2. Write the function contract
3. Write the function purpose
4. Provide tests
5. Provide an implementation
|#

#| P1 |#

#| Parte A |#
#|
<Prop> :: = (varp <String>)
        |   (andp <Prop> <Prop>)
        |   (orp <Prop> <Prop>)
        |   (notp <Prop>)
|#
;; Defines the recursive data type "Prop" with its grammar.

(deftype Prop
    (varp n)
    (andp p q)
    (orp p q)
    (notp p))

#| Parte B |#
;; occurrences :: Prop String -> Number
;; Returns the number of occurrences of a variable.

(define (occurrences p s) 
    (match p
        [(varp q) 
            (cond
                [(equal? q s) 1]
                [else 0])]
        [(andp q r) 
            (+ (occurrences q s) (occurrences r s))]
        [(orp q r) 
            (+ (occurrences q s) (occurrences r s))]
        [(notp q) 
            (+ (occurrences q s) )]))
        
#| Parte C |#
;; vars :: Prop -> (Listof String)
;; Returns a list of unique variables in the proposition.

(define (vars p)
    (match p
        [(varp q) 
            (list q)]
        [(andp q r) 
            (remove-duplicates(append (vars q) (vars r)))]
        [(orp q r) 
            (remove-duplicates(append (vars q) (vars r)))]
        [(notp q) 
            (vars q)])) 

#| Parte D |#

;; Auxiliary function
;; rec-enviroments :: String (Listof (Listof (Pair String Boolean))) -> (Listof (Listof (Pair String Boolean)))
;; Returns a list with adding a new enviroment.  

(define (rec-enviroments s acc)
    (define x (list (cons s #t)))
    (define y (list (cons s #f)))
    (append (map (lambda (ls) (append x ls)) acc) 
            (map (lambda (ls) (append y ls)) acc)))


;; all-environments :: (Listof String) -> (Listof (Listof (Pair String Boolean)))
;; Returns a list of all the possibles enviroments.

(define (all-environments l) 
    (foldr rec-enviroments '(()) l))

#| Parte E |#

;; eval :: Prop (Listof (Pair String Boolean)) -> Boolean

#| Parte F |#

;; tautology? :: Prop -> Boolean



#| P2 |#

#| Parte A |#

;; simplify-negations :: Prop -> Prop

#| Parte B |#

;; distribute-and :: Prop -> Prop

#| Parte C |#

;; apply-until :: (a -> a) (a a -> Boolean) -> a -> a

#| Parte D |#

;; DNF :: Prop -> Prop



#| P3 |#

#| Parte A |#

;; fold-prop :: (String -> a) (a a -> a) (a a -> a) (a -> a) -> Prop -> a

#| Parte B |#

;; occurrences-2 :: Prop String -> Number

;; vars-2 :: Prop -> (Listof String)

;; eval-2 :: Prop (Listof (Pair String Boolean)) -> Boolean

;; simplify-negations-2 :: Prop -> Prop

;; distribute-and-2 :: Prop -> Prop
