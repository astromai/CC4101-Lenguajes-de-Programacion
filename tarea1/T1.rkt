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
    (notp p)
)

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
            (+ (occurrences q s) )])
)
        
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
            (vars q)])
) 

#| Parte D |#

;; Auxiliary function
;; rec-enviroments :: String (Listof (Listof (Pair String Boolean))) -> (Listof (Listof (Pair String Boolean)))
;; Returns a list with adding a new enviroment.  

(define (rec-enviroments s acc)
    (define x (list (cons s #t)))
    (define y (list (cons s #f)))
    (append (map (lambda (ls) (append x ls)) acc) 
            (map (lambda (ls) (append y ls)) acc))
)

;; all-environments :: (Listof String) -> (Listof (Listof (Pair String Boolean)))
;; Returns a list of all the possibles enviroments.

(define (all-environments l) 
    (foldr rec-enviroments '(()) l)
)

#| Parte E |#

;; eval :: Prop (Listof (Pair String Boolean)) -> Boolean
;; Returns an evaluation for the proposition.

(define (eval p l)
    (match p
        [(varp q) 
            (cond
                [(assoc q l) (cdr (assoc q l))]
                [else (error 'eval (format "variable ~a is not defined in environment" q))])]
        [(andp q r) 
            (and (eval q l) (eval r l))]
        [(orp q r) 
            (or (eval q l) (eval r l))]
        [(notp q) 
            (not (eval q l))]) 
)

#| Parte F |#

;; tautology? :: Prop -> Boolean
;; Return if an evaluation could be a tautology in any enviroment.

(define (tautology? p)
    (define l (vars p))
    (define lst (all-environments l))
    (foldl 
        (lambda (x acc) 
            (and acc (eval p x))) 
        #t 
        lst)
)

#| P2 |#

#| Parte A |#

;; simplify-negations :: Prop -> Prop
;; Return a simplified proposition of negations.

(define (simplify-negations p)
    (match p
        ;; Base case
        [(varp q) (varp q)]
        [(notp (varp q)) (notp (varp q))]

        ;; Double Negation
        [(notp (notp q))
            (simplify-negations q)]

        ;; Morgan Law    
        [(notp (andp q r)) 
            (orp (notp (simplify-negations  q)) (notp (simplify-negations r)))]
        [(notp (orp q r))
            (andp (notp (simplify-negations  q)) (notp (simplify-negations r)))]

        ;; Inductive case
        [(andp q r)
            (andp (simplify-negations q) (simplify-negations r))]
        [(orp q r)
            (orp (simplify-negations q) (simplify-negations r))])
)

#| Parte B |#

;; distribute-and :: Prop -> Prop
;; Return a proposition with transformations applied in a single pass over its conjunctions.

(define (distribute-and k)
    (match k
        ;; Base case
        [(varp q) (varp q)]
        [(notp (varp q)) (notp (varp q))]

        ;; Distribution
        [(andp p (orp q r)) 
            (orp 
                (andp (distribute-and p) (distribute-and q)) 
                (andp (distribute-and p) (distribute-and r)))]
        [(andp (orp p q) r)     
            (orp 
                (andp (distribute-and p) (distribute-and r)) 
                (andp (distribute-and q) (distribute-and r)))]

        ;; Inductive case
        [(notp p) 
            (notp (distribute-and p))]
        [(orp p q) 
            (orp (distribute-and p) (distribute-and q))]
        [(andp p q) 
            (andp (distribute-and p) (distribute-and q))]))


#| Parte C |#

;; apply-until :: (a -> a) (a a -> Boolean) -> a -> a
;; Returns a function that repeatedly applies f until the predicate p between
;; the last two results is true

(define (apply-until f p)
  (lambda (x)
    (define (recurse prev)
      (let ((curr (f prev)))
        (if (p prev curr)
            curr
            (recurse curr))))
    (recurse x)))

#| Parte D |#

;; DNF :: Prop -> Prop

(define (DNF p)
    (define y ((apply-until simplify-negations equal?) p))
    (define x ((apply-until distribute-and equal?) y))
    x
)

#| P3 |#

#| Parte A |#

;; fold-prop :: (String -> a) (a a -> a) (a a -> a) (a -> a) -> Prop -> a

(define (fold-prop f g h s)
    (lambda (p)
        (match p
            [(varp q) (f q)]
            [(andp q r) (g 
                            ((fold-prop f g h s) q)
                            ((fold-prop f g h s) r))]
            [(orp q r) (h 
                            ((fold-prop f g h s) q)
                            ((fold-prop f g h s) r))]
            [(notp q) (s
                            ((fold-prop f g h s) q))]))
)

#| Parte B |#

;; occurrences-2 :: Prop String -> Number
;; Returns the number of occurrences of a variable.

(define (occurrences-2 p s)
    ((fold-prop 
        (lambda (v) (if (equal? v s) 1 0))
        (lambda (v l) (+ v l))
        (lambda (v l) (+ v l))
        (lambda (v) v)) 
    p)
)

;; vars-2 :: Prop -> (Listof String)
;; Returns a list of unique variables in the proposition.

(define (vars-2 p)
    ((fold-prop 
        (lambda (v) (list v))
        (lambda (v l) (remove-duplicates(append v l)))
        (lambda (v l) (remove-duplicates(append v l)))
        (lambda (v) v)) 
    p)
)

;; eval-2 :: Prop (Listof (Pair String Boolean)) -> Boolean
;; Returns an evaluation for the proposition.

(define (eval-2 p l)
    ((fold-prop 
        (lambda (v) (cond
                        [(assoc v l) (cdr (assoc v l))]
                        [else (error 'eval (format "variable ~a is not defined in environment" v))]))
        (lambda (v t) (and v t))
        (lambda (v t) (or v t))
        (lambda (v) (not v))) 
    p)
)

;; simplify-negations-2 :: Prop -> Prop
;; Return a simplified proposition of negations.

(define (simplify-negations-2 p)
    ((fold-prop
        (lambda (v) (varp v))
        (lambda (l r) (andp l r))
        (lambda (l r) (orp l r))
        (lambda (v)
            (match v
                [(notp (notp y)) y]                 
                [(andp a b) (orp (notp a) (notp b))] 
                [(orp a b)  (andp (notp a) (notp b))]
                [(notp (varp x)) (varp x)] 
                [else (notp v)]))
                )
    p)
)

;; distribute-and-2 :: Prop -> Prop
;; Return a proposition with transformations applied in a single pass over its conjunctions.

(define (distribute-and-2 p)
  ((fold-prop
     (lambda (v) (varp v))
     (lambda (l r)
        (match r
            [(orp a b) (orp (andp l a) (andp l b))]
            [else
                (match l
                    [(orp a b) (orp (andp a r) (andp b r))]
                    [else (andp l r)])]))
     (lambda (l r) (orp l r))
     (lambda (v) (notp v)))
   p)
)