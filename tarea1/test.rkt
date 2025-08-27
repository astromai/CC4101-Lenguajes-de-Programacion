#lang play
(require "T1.rkt")

(print-only-errors #t)

;; Test (occurrences)
(test (occurrences (varp "a") "b") 0)
(test (occurrences (notp (varp "a")) "b") 0)
(test (occurrences (notp (varp "a")) "a") 1)
(test (occurrences (andp (varp "a") (varp "b")) "a") 1)
(test (occurrences (andp (varp "a") (varp "a")) "a") 2)
(test (occurrences (notp (andp (varp "a") (varp "c"))) "a") 1)

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

