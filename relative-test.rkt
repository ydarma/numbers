#lang racket
(require rackunit)
(require rackunit/text-ui)
(require (prefix-in n: "natural.rkt"))
(require "relative.rkt")

(run-tests
 (test-suite
  "Relative number tests"
  (test-case
   "Division by zero"
   (check-exn
    exn:fail?
    (λ ()
      (div (relative '- (n:natural 27)) (relative '+ (n:natural 0)))
      )
    )
   )
  (test-case
   "Division with two negative relative numbers"
   (check-equal?
    (div (relative '- (n:natural 27)) (relative '- (n:natural 4)))
    (divresult (relative '+ (n:natural 6)) (relative '- (n:natural 3)))
    )
   )
  )
 )