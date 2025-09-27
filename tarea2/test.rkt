#lang play

#|
Nombre: Tomás Ubilla Zavala
|#

(require "T2.rkt")
(print-only-errors #t)


;; P1
;; Test remove-trailing-zeros
(test
  (remove-trailing-zeros '(3 1 0 0 0 0))  
  '(3 1)
)   
(test
  (remove-trailing-zeros '(3 1 2 3 0 3 4))
  '(3 1 2 3 0 3 4)
)   
(test
  (remove-trailing-zeros '(0 0 0))        
  '(0)
) 
(test
  (remove-trailing-zeros '(1 0 2 0 0))    
  '(1 0 2)
)


;; Test parse

  ;; atom number
(test 
  (parse 0)
  (poly '(0))
)
  
  ;; atom symbol
(test
  (parse 'x)
  (id 'x)
)  
  ;; list of numbers
(test
  (parse '(3 1 0 0))
  (poly '(3 1))
)
  ;; operator +
(test
  (parse '(+ (3 5 7) (1 2)))
  (add (poly '(3 5 7)) (poly '(1 2)))
)
(test
  (parse '(+ (3 5 7) (1 2) (5 5 0 4 0 0 2 0 0 0 0 0)))
  (add (add (poly '(3 5 7)) (poly '(1 2))) (poly '(5 5 0 4 0 0 2)))
)
  ;; operator *
(test
  (parse '(*(2 1) (1 2)))
  (mul (poly '(2 1)) (poly '(1 2)))
)
(test
  (parse '(* (3 5 7) (1 2) (5 5 0 4 0 0 2 0 0 0 0 0)))
  (mul (mul (poly '(3 5 7)) (poly '(1 2))) (poly '(5 5 0 4 0 0 2)))
)
  ;; operator if0
(test 
  (parse '(if0(+ (3 5) (-3 2)) 1 0))
  (if0 (add (poly '(3 5)) (poly '(-3 2))) (poly '(1)) (poly '(0)))
)
  ;; operator with
(test
  (parse '(with x (1 2) (* 2 x)))
  (with 'x (poly '(1 2)) (mul (poly '(2)) (id 'x)))
)

;; Test reduce

;; ====================
;; Test reduce

;; poly literal
(test
  (reduce (poly '(1 2 3)) empty-env)
  (poly '(1 2 3))
)

;; add of two polys
(test
  (reduce (add (poly '(1 2 3)) (poly '(4 5 6))) empty-env)
  (poly '(5 7 9))
)

;; mul of two polys
(test
  (reduce (mul (poly '(1 2)) (poly '(3 4))) empty-env)
  (poly '(3 8))
)

;; id lookup in env
(test
  (reduce (id 'y) (extend-env 'y (poly '(1)) empty-env))
  (poly '(1))
)

;; id not in env
(test/exn
  (reduce (id 'y) empty-env)
  "reduce: variable y is not defined"
)
  
;; if0 true branch
(test
  (reduce (if0 (poly '(0)) (poly '(5 6)) (poly '(7 8))) empty-env)
  (poly '(5 6))
)

;; if0 false branch
(test
  (reduce (if0 (poly '(1)) (poly '(5 6)) (poly '(7 8))) empty-env)
  (poly '(7 8))
)

;; with single binding using extend-env
(test
  (let ([env1 (extend-env 'x (poly '(2 3)) empty-env)])
    (reduce (add (id 'x) (poly '(4 5))) env1))
  (poly '(6 8))
)

;; with multiple bindings using extend-env
(test
  (let* ([env1 (extend-env 'x (poly '(2 3)) empty-env)]
         [env2 (extend-env 'y (poly '(1 1)) env1)])
    (reduce (add (id 'x) (id 'y)) env2))
  (poly '(3 4))
)

;; with shadowing using extend-env
(test
  (let* ([env1 (extend-env 'x (poly '(2 3)) empty-env)]
         [env2 (extend-env 'x (poly '(4 5)) env1)])
    (reduce (add (id 'x) (poly '(1 1))) env2))
  (poly '(5 6))
)

;; complex nested with and arithmetic using extend-env
(test
  (let* ([env1 (extend-env 'x (poly '(1 2)) empty-env)]
         [env2 (extend-env 'y (poly '(3 4)) env1)])
    (reduce (add (mul (id 'x) (id 'y))
                 (poly '(1 1))) env2))
  (poly '(4 9))
)




;; ====================

;; P2

;; Test parser

  ;; atom number
(test
  (parser 5)
  (real 5)
)
  ;; atom imaginary
(test
  (parser '(4 i))
  (imaginary 4)
)
  ;; atom idc
(test
  (parser 'x)
  (idc 'x)
)
  ;; operator + 
(test
  (parser '(+ 2 (1 i)))
  (addc (real 2) (imaginary 1))
)
  ;; operator -
(test
  (parser '(- 2 (1 i)))
  (subc (real 2) (imaginary 1))
)
  ;; withc 
(test
  (parser '(with [(x 1) (y 1)] (+ x y)))
  (withc 
    (list 
      (cons 'x (real 1)) 
      (cons 'y (real 1))) 
    (addc (idc 'x) (idc 'y))
  )
)
(test
  (parser '(with () (+ x y)))
  "parser: *with* expects at least one definition"
)


;; Test cvalue functions

  ;; from-CValue
(test 
  (from-CValue (compV 4 20))
  (addc (real 4) (imaginary 20))
)
  ;; cmplx+
(test
  (cmplx+ (compV 2 2) (compV 3 3))
  (compV 5 5)
)
  ;; cmplx-
(test
  (cmplx- (compV 2 2) (compV 3 3))
  (compV -1 -1)
)
  ;; cmplx0?
(test
  (cmplx0? (compV 0 0))
  #t
)
(test
  (cmplx0? (compV 0 1))
  #f
)


;; Test subst
  ;; sin shadowing
(test
  (subst (parser '(with [(x 2) (y z)] (+ x z))) 'z (real 1))
  (withc (list (cons 'x (real 2))
               (cons 'y (real 1)))
         (addc (idc 'x) (real 1)))  
)
  ;; shadowing
(test
  (subst (parser '(with [(x 2) (y x)] (+ x x))) 'x (real 1))
  (withc (list (cons 'x (real 2))
               (cons 'y (idc 'x)))
         (addc (idc 'x) (idc 'x)))
)

;; Test interp
;; real number
(test
  (interp (real 5))
  (compV 5 0)
)

;; imaginary number
(test
  (interp (imaginary 4))
  (compV 0 4)
)

;; idc 
(test/exn
  (interp (idc 'x))
  "Free occurrence of a variable"
)

;; addc
(test
  (interp (addc (real 2) (imaginary 3)))
  (compV 2 3)
)

;; subc
(test
  (interp (subc (real 5) (imaginary 1)))
  (compV 5 -1)
)

;; if0c true branch
(test
  (interp (if0c (real 0) (real 42) (real 99)))
  (compV 42 0)
)

;; if0c false branch
(test
  (interp (if0c (real 1) (real 42) (real 99)))
  (compV 99 0)
)


#| estos de aca fallan
;; withc single binding
(test
  (interp (withc (list (cons 'x (real 5)))
                (addc (idc 'x) (real 3))))
  (compV 8 0)
)

;; withc multiple bindings
(test
  (interp (withc (list (cons 'x (real 2))
                       (cons 'y (addc (idc 'x) (real 1))))
                (addc (idc 'x) (idc 'y))))
  (compV 5 0)
)

;; withc with shadowing
(test
  (interp (withc (list (cons 'x (real 2))
                       (cons 'y (idc 'x)))
                (addc (idc 'x) (idc 'y))))
  (compV 4 0)
)
|#