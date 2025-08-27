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
