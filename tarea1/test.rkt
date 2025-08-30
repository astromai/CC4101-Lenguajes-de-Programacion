#lang play
(require "T1.rkt")

(print-only-errors #t)

;; Test (occurrences)
(test 
  (occurrences (varp "a") "b") 
  0)
(test 
  (occurrences (notp (varp "a")) "b") 
  0)
(test 
  (occurrences (notp (varp "a")) "a") 
  1)
(test 
  (occurrences (andp (varp "a") (varp "b")) "a") 
  1)
(test 
  (occurrences (andp (varp "a") (varp "a")) "a") 
  2)
(test 
  (occurrences (notp (andp (varp "a") (varp "c"))) "a") 
  1)

;; Test (vars)
(test (vars (varp "a")) '("a"))
(test (vars (notp (varp "a"))) '("a"))
(test (vars (andp (varp "a") (varp "b"))) '("a" "b"))
(test (vars (andp (varp "a") (varp "a"))) '("a"))
(test (vars (notp (andp (varp "a") (varp "c")))) '("a" "c"))

;; Test (rec-enviroments)
(test 
  (rec-enviroments "a" '(()))
  (list 
    ( list (cons "a" #t))
    ( list (cons "a" #f))))
(test 
  (rec-enviroments "b" (rec-enviroments "a" '(())))
  (list 
    ( list (cons "b" #t) (cons "a" #t))
    ( list (cons "b" #t) (cons "a" #f))
    ( list (cons "b" #f) (cons "a" #t))
    ( list (cons "b" #f) (cons "a" #f))))
(test 
  (rec-enviroments "c" (rec-enviroments "b" (rec-enviroments "a" '(()))))
  (list 
    ( list (cons "c" #t) (cons "b" #t) (cons "a" #t))
    ( list (cons "c" #t) (cons "b" #t) (cons "a" #f))
    ( list (cons "c" #t) (cons "b" #f) (cons "a" #t))
    ( list (cons "c" #t) (cons "b" #f) (cons "a" #f))
    ( list (cons "c" #f) (cons "b" #t) (cons "a" #t))
    ( list (cons "c" #f) (cons "b" #t) (cons "a" #f))
    ( list (cons "c" #f) (cons "b" #f) (cons "a" #t))
    ( list (cons "c" #f) (cons "b" #f) (cons "a" #f))))

;; Test (all-environments)
(test 
  (all-environments '()) 
  '(()))
(test 
  (all-environments ( list "a")) 
  (list
    ( list (cons "a" #t))
    ( list (cons "a" #f))))
(test 
  (all-environments ( list "a" "b"))
  (list   
    ( list (cons "a" #t) (cons "b" #t))
    ( list (cons "a" #t) (cons "b" #f))
    ( list (cons "a" #f) (cons "b" #t))
    ( list (cons "a" #f) (cons "b" #f))))
(test
  (all-environments (list "a" "b" "c"))
  (list
    ( list (cons "a" #t) (cons "b" #t) (cons "c" #t))
    ( list (cons "a" #t) (cons "b" #t) (cons "c" #f))
    ( list (cons "a" #t) (cons "b" #f) (cons "c" #t))
    ( list (cons "a" #t) (cons "b" #f) (cons "c" #f))
    ( list (cons "a" #f) (cons "b" #t) (cons "c" #t))
    ( list (cons "a" #f) (cons "b" #t) (cons "c" #f))
    ( list (cons "a" #f) (cons "b" #f) (cons "c" #t))
    ( list (cons "a" #f) (cons "b" #f) (cons "c" #f))))

;; Test (eval)
(test 
  (eval 
    (andp (varp "a") (varp "a")) 
    (list (cons "a" #t)))
  #t)
(test 
  (eval 
    (notp (andp (varp "a") (varp "c"))) 
    (list (cons "a" #f) (cons "c" #f)))
  #t)
(test 
  (eval 
    (andp (varp "a") (varp "b")) 
    (list (cons "a" #f) (cons "b" #f)))
  #f)  
(test/exn
  (eval 
    (andp (varp "a") (varp "a"))
    (list (cons "b" #t)))
  "eval: variable a is not defined in environment")  

;; Test (tautology?)
(test 
  (tautology? (notp (andp (varp "a") (varp "c")))) 
  #f) 
(test 
  (tautology? (orp (varp "a") (notp (varp "a")))) 
  #t)     
(test
  (tautology? 
    (andp 
      (orp (varp "a") (notp (varp "a"))) 
      (orp (varp "b") (notp (varp "b")))))
  #t)
(test 
  (tautology? (andp (varp "a") (varp "b")))
  #f)
(test 
  (tautology? 
    (or 
      (andp (varp "a") (varp "b"))
      (andp (notp (varp "b")) (varp "c"))))
  #f)    

;; Test (simplify-negations)
(test 
  (simplify-negations
    (notp (notp (varp "a"))))
  (varp "a"))
(test 
  (simplify-negations 
    (notp (notp (andp (varp "a") (varp "b")))))
  (andp (varp "a") (varp "b")))  
(test 
  (simplify-negations
    (notp (andp (varp "a") (varp "b"))))
  (orp (notp (varp "a")) (notp (varp "b"))))
(test 
  (simplify-negations
    (notp (orp (varp "a") (varp "b"))))
  (andp (notp (varp "a")) (notp (varp "b"))))
(test 
  (simplify-negations
    (notp (orp (notp (varp "a")) (varp "b"))))
  (andp (notp (notp (varp "a"))) (notp (varp "b")))) 
(test
  (simplify-negations 
    (andp 
      (notp (notp (varp "a"))) 
      (notp (notp (varp "b")))))
  (andp 
    (varp "a")
    (varp "b"))) 

;; Test (distribute-and)
(test
  (distribute-and 
    (andp 
      (orp (varp "a") (varp "b")) 
      (varp "c")))
  (orp 
    (andp (varp "a") (varp "c")) 
    (andp (varp "b") (varp "c"))))
(test
  (distribute-and 
    (andp 
      (varp "c") 
      (orp (varp "a") (varp "b"))))
  (orp 
    (andp (varp "c") (varp "a")) 
    (andp (varp "c") (varp "b"))))
(test
  (distribute-and
    (andp 
      (orp (varp "a") (varp "b"))
      (orp (varp "c") (varp "d"))))
    (orp 
      (andp (orp (varp "a") (varp "b")) (varp "c"))
      (andp (orp (varp "a") (varp "b")) (varp "d"))))
(test
  (distribute-and
    (andp (varp "x")
          (andp (orp (varp "y") (varp "z")) (varp "w"))))
  (andp (varp "x")
        (orp (andp (varp "y") (varp "w"))
             (andp (varp "z") (varp "w")))))
(test
  (distribute-and
    (andp (notp (varp "p"))
          (orp (varp "q") (varp "r"))))
  (orp
    (andp (notp (varp "p")) (varp "q"))
    (andp (notp (varp "p")) (varp "r"))))
(test
  (distribute-and
    (andp (orp (varp "a") (varp "b"))
          (andp (varp "c") (orp (varp "d") (varp "e")))))
  (orp
    (andp (varp "a") (orp (andp (varp "c") (varp "d")) (andp (varp "c") (varp "e"))))
    (andp (varp "b") (orp (andp (varp "c") (varp "d")) (andp (varp "c") (varp "e"))))))

;; Test (apply-until)
(test 
  ((apply-until
    (\lambda (x) (/ x (add1 x)))
    (\lambda (x new-x) (<= (- x new-x) 0.1))) 
  1)
  0.25)
(test
  ((apply-until
     (lambda (x) (* 0.5 x))            
     (lambda (x new-x) (<= (- x new-x) 0.05))) 
   1)                                       
  0.03125)                                  


;; Test (DNF)
(test 
  (DNF 
    (andp (orp (varp "a") (varp "b")) (orp (varp "c") (varp "d"))))
  (orp 
    (orp (andp (varp "a") (varp "c")) (andp (varp "b") (varp "c"))) 
    (orp (andp (varp "a") (varp "d")) (andp (varp "b") (varp "d")))))
(test
  (DNF 
    (andp 
      (orp 
        (notp (notp (varp "p"))) 
        (varp "q"))
      (orp 
        (varp "r") 
        (notp (notp (varp "s"))))))
  (orp
    (orp
      (andp (varp "p") (varp "r"))
      (andp (varp "q") (varp "r")))
    (orp
      (andp (varp "p") (varp "s"))
      (andp (varp "q") (varp "s")))))
(test
  (DNF
    (andp
      (orp (notp (notp (notp (varp "p"))))
           (orp (varp "q") (notp (notp (notp (varp "r"))))))
      (orp (varp "x") (orp (varp "y") (varp "z")))))
  (orp
    (orp
      (andp (notp (varp "p")) (varp "x"))
      (orp
        (andp (varp "q") (varp "x"))
        (andp (notp (varp "r")) (varp "x"))))
    (orp
      (orp
        (andp (notp (varp "p")) (varp "y"))
        (orp
          (andp (varp "q") (varp "y"))
          (andp (notp (varp "r")) (varp "y"))))
      (orp
        (andp (notp (varp "p")) (varp "z"))
        (orp
          (andp (varp "q") (varp "z"))
          (andp (notp (varp "r")) (varp "z")))))))

;; Test (fold-prop)

;; occurrences-2

(test
  (equal?
    (occurrences (varp "a") "b")
    (occurrences-2 (varp "a") "b"))
  #t)
(test
  (equal?
    (occurrences (notp (varp "a")) "b")
    (occurrences-2 (notp (varp "a")) "b"))
  #t)
(test
  (equal?
    (occurrences (notp (varp "a")) "a")
    (occurrences-2 (notp (varp "a")) "a"))
  #t)
(test
  (equal?
    (occurrences (andp (varp "a") (varp "b")) "a")
    (occurrences-2 (andp (varp "a") (varp "b")) "a"))
  #t)
(test
  (equal?
    (occurrences (andp (varp "a") (varp "a")) "a")
    (occurrences-2 (andp (varp "a") (varp "a")) "a"))
  #t)
(test
  (equal?
    (occurrences (notp (andp (varp "a") (varp "c"))) "a")
    (occurrences-2 (notp (andp (varp "a") (varp "c"))) "a"))
  #t)
(test
  (equal?
    (occurrences (andp (varp "a") (varp "a")) "a")
    (occurrences-2 (andp (varp "a") (varp "a")) "a"))
  #t)

;; vars-2
(test
  (equal?
    (vars (varp "a"))
    (vars-2 (varp "a")))
  #t)
(test
  (equal?
    (vars (notp (varp "a")))
    (vars-2 (notp (varp "a"))))
  #t)
(test
  (equal?
    (vars (andp (varp "a") (varp "b")))
    (vars-2 (andp (varp "a") (varp "b"))))
  #t)
(test
  (equal?
    (vars (andp (varp "a") (varp "a")))
    (vars-2 (andp (varp "a") (varp "a"))))
  #t)
(test
  (equal?
    (vars (notp (andp (varp "a") (varp "c"))))
    (vars-2 (notp (andp (varp "a") (varp "c")))))
  #t)

;; eval-2
(test
  (equal?
    (eval (andp (varp "a") (varp "a")) (list (cons "a" #t)))
    (eval-2 (andp (varp "a") (varp "a")) (list (cons "a" #t))))
  #t)
(test
  (equal?
    (eval (notp (andp (varp "a") (varp "c"))) (list (cons "a" #f) (cons "c" #f)))
    (eval-2 (notp (andp (varp "a") (varp "c"))) (list (cons "a" #f) (cons "c" #f))))
  #t)
(test
  (equal?
    (eval (andp (varp "a") (varp "b")) (list (cons "a" #f) (cons "b" #f)))
    (eval-2 (andp (varp "a") (varp "b")) (list (cons "a" #f) (cons "b" #f))))
  #t)
(test/exn
  (eval-2 
    (andp (varp "a") (varp "a"))
    (list (cons "b" #t)))
  "eval: variable a is not defined in environment")  


;; simplify-negations-2
(test
  (equal?
    (simplify-negations (notp (notp (varp "a"))))
    (simplify-negations-2 (notp (notp (varp "a")))))
  #t)
(test
  (equal?
    (simplify-negations (simplify-negations (notp (notp (andp (varp "a") (varp "b"))))))
    (simplify-negations-2 (simplify-negations-2 (notp (notp (andp (varp "a") (varp "b")))))))
  #t)
(test
  (equal?
    (simplify-negations (notp (andp (varp "a") (varp "b"))))
    (simplify-negations-2 (notp (andp (varp "a") (varp "b")))))
  #t)
(test
  (equal?
    (simplify-negations (notp (orp (varp "a") (varp "b"))))
    (simplify-negations-2 (notp (orp (varp "a") (varp "b")))))
  #t)
(test
  (equal?
    (simplify-negations (notp (orp (notp (varp "a")) (varp "b"))))
    (simplify-negations-2 (notp (orp (notp (varp "a")) (varp "b")))))
  #t)
(test
  (equal?
    (simplify-negations (andp (notp (notp (varp "a"))) (notp (notp (varp "b")))))
    (simplify-negations-2 (andp (notp (notp (varp "a"))) (notp (notp (varp "b"))))))
  #t)

;; distribute-and-2
(test
  (equal?
    (distribute-and 
      (andp 
        (orp (varp "a") (varp "b")) 
        (varp "c")))
    (distribute-and-2 
      (andp 
        (orp (varp "a") (varp "b")) 
        (varp "c"))))
  #t)
(test
  (equal?
    (distribute-and 
      (andp 
        (varp "c") 
        (orp (varp "a") (varp "b"))))
    (distribute-and-2 
      (andp 
        (varp "c") 
        (orp (varp "a") (varp "b")))))
  #t)
(test
  (equal?
    (distribute-and (distribute-and
      (andp 
        (orp (varp "a") (varp "b"))
        (orp (varp "c") (varp "d")))))
    (distribute-and-2 (distribute-and-2
      (andp 
        (orp (varp "a") (varp "b"))
        (orp (varp "c") (varp "d"))))))
  #t)
(test
  (equal?
    (distribute-and(distribute-and
      (andp (varp "x")
            (andp (orp (varp "y") (varp "z")) (varp "w")))))
    (distribute-and-2(distribute-and-2
      (andp (varp "x")
            (andp (orp (varp "y") (varp "z")) (varp "w"))))))
  #t)
(test
  (equal?
    (distribute-and
      (andp (notp (varp "p"))
            (orp (varp "q") (varp "r"))))
    (distribute-and-2
      (andp (notp (varp "p"))
            (orp (varp "q") (varp "r")))))
  #t)

;; Test particular, netamente problemas de orden de terminos pero booleanamente iguales
#|
=> v1
(orp
 (orp
  (andp (varp "a") (andp (varp "c") (varp "d")))
  (andp (varp "a") (andp (varp "c") (varp "e"))))
 (orp
  (andp (varp "b") (andp (varp "c") (varp "d")))
  (andp (varp "b") (andp (varp "c") (varp "e")))))

=>  v2
(orp
 (orp
  (andp (varp "a") (andp (varp "c") (varp "d")))
  (andp (varp "b") (andp (varp "c") (varp "d"))))
 (orp
  (andp (varp "a") (andp (varp "c") (varp "e")))
  (andp (varp "b") (andp (varp "c") (varp "e")))))
|#
(test
  (equal?
    (distribute-and(distribute-and
      (andp (orp (varp "a") (varp "b"))
            (andp (varp "c") (orp (varp "d") (varp "e"))))))
    (distribute-and-2(distribute-and-2
      (andp (orp (varp "a") (varp "b"))
            (andp (varp "c") (orp (varp "d") (varp "e")))))))
  #f)

