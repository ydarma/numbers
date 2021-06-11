#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "natural.rkt")

(run-tests
 (test-suite
  "Relative number tests"
  (test-case
   "Natural value is a non negative integer"
   (check-exn
    exn:fail:contract?
    (lambda ()(natural -3))
    )
   )
  (test-case
   "Division with two negative relative numbers"
   (check-equal?
    (div (natural 27) (natural 4))
    (divresult (natural 6) (natural 3))
    )
   )
  )
 )