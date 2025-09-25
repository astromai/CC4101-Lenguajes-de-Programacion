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

;; P2

;; Test parse

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
  (parser '(with () (+ x y)))
  "parser: *with* expects at least one definition"
)
