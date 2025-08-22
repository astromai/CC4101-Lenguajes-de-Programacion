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
